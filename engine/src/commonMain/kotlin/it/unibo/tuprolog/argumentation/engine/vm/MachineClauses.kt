package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.ClauseObservers
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_CALLS
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_KEY
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_NEXT
import it.unibo.tuprolog.argumentation.engine.vm.program.ClauseCode
import it.unibo.tuprolog.argumentation.engine.vm.program.ClauseTable
import it.unibo.tuprolog.argumentation.engine.vm.program.Predicate
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag

// The clauses of a predicate a goal may match, selected by the keys of its arguments (the first, the others, those within
// the first) and by the indexes of large predicates; the first entered, a choice point delayed for the next one.
//
// The loops over the clauses of a table and over the candidates of an index stay apart (tryAmongClauses and
// tryCandidates, nextCandidate and nextPosition): one loop over either, the candidates null for the clauses in order,
// measured 2% slower (PERFORMANCE_NOTES, 2026-11-10)

// The key of a word dereferenced: its functor, the key of lists, the constant itself; 0 for a variable, matching any key
private fun Machine.keyOf(d: Int): Int =
    when (tag(d)) {
        REF -> 0
        STR -> heap.cells[payload(d)]
        LIST -> Words.LIST_KEY
        else -> d
    }

// The key of the argument [k] of the goal, left dereferenced for the head to find it so
internal fun Machine.keyOfArgument(k: Int): Int {
    val d = heap.dereferenced(regs[k])
    regs[k] = d
    return keyOf(d)
}

// The predicate of [functor] the resolution sees: the theory of the user's, if visible and it defines it (the
// theory is a module of its own, whatever the libraries define under the same name), else the libraries'
internal fun Machine.predicateOf(functor: Int): Predicate? =
    (if (theoryVisible) program.theory.predicate(functor) else null) ?: scope.predicate(functor)

// Enters the first clause of [predicate] the goal on the registers may match, leaving a choice point if another
// may too (by the keys of every argument of the clauses: no trailing No for a clause whose head could not unify, as
// BurdenOfProofTest counts solutions); false when none does
internal fun Machine.tryClauses(
    predicate: Predicate,
    arity: Int,
    barrier: Int,
): Boolean {
    val table = predicate.table
    val n = table.size
    // (one clause: its head decides, with no key to look at)
    if (n == 1) return entered(table, 0, barrier)
    val key = if (arity > 0) keyOfArgument(0) else 0
    val firstKeys = table.firstKeys
    if (n == 2 && key != 0) {
        // (two clauses with two first keys, as [] and [_|_]: the key decides, with no alternative to look for)
        val firstKey = firstKeys[0]
        val secondKey = firstKeys[1]
        if (firstKey != 0 && secondKey != 0 && firstKey != secondKey) {
            return when (key) {
                firstKey -> entered(table, 0, barrier)
                secondKey -> entered(table, 1, barrier)
                else -> false
            }
        }
    }
    return tryAmongClauses(predicate, table, key, arity, barrier)
}

// [tryClauses] when more than the first key may be needed
private fun Machine.tryAmongClauses(
    predicate: Predicate,
    table: ClauseTable,
    key: Int,
    arity: Int,
    barrier: Int,
): Boolean {
    val n = table.size
    val firstKeys = table.firstKeys
    if (n >= Predicate.INDEXED_FROM) {
        val candidates = selected(predicate, key, arity)
        if (candidates != null) return tryCandidates(table, candidates, predicate.functor, arity, barrier, key)
    }
    // The clauses in order, over the flat tables: the first candidate by its first argument, its head deciding the
    // rest (by the keys within it too, if the clauses have them: the lookahead would not tell them apart); the
    // lookahead for another by every argument the clause keys
    var i = 0
    if (table.innerDeep) {
        while (i < n && !matches(table.clauses[i]!!, key, arity)) i++
    } else {
        while (i < n && !keyMatches(firstKeys[i], key)) i++
    }
    if (i == n) return false
    val clause = table.clauses[i]!!
    if (i + 1 < n && !commits(clause)) {
        // (a guarded clause either fails, its alternatives then looked at closely, or cuts them)
        val free = !clause.guarded && table.deep && !table.innerDeep && laterArgumentsFree(arity)
        val j = nextCandidate(table, i + 1, key, arity, free || clause.guarded)
        if (j < n) {
            delayChoicePoint(table, null, predicate.functor, clause, j, arity, key)
            delayedFree = free
        }
    }
    return entered(table, i, barrier)
}

// Whether the clause entered goes past its head for sure, its neck cut then dropping any alternative
private fun Machine.commits(clause: ClauseCode): Boolean {
    val mask = clause.commitMask
    if (mask < 0) return false
    var bits = mask
    var k = 0
    while (bits != 0) {
        if (bits and 1 != 0 && tag(heap.deref(regs[k])) != REF) return false
        bits = bits ushr 1
        k++
    }
    return true
}

// Whether no argument of the goal beyond the first is bound: no key of a clause there can exclude it
private fun Machine.laterArgumentsFree(arity: Int): Boolean {
    var k = 1
    while (k < arity) {
        if (tag(heap.deref(regs[k])) != REF) return false
        k++
    }
    return true
}

private fun Machine.entered(
    table: ClauseTable,
    clause: Int,
    barrier: Int,
): Boolean {
    cutBarrier = barrier
    if (ClauseObservers.active) ClauseObservers.resolved(table.clauses[clause]!!.source)
    pc = table.starts[clause]
    return true
}

// The clauses of a large [predicate] the goal on the registers may match, if an index says
private fun Machine.selected(
    predicate: Predicate,
    key: Int,
    arity: Int,
): IntArray? = if (key != 0) narrowed(predicate, predicate.candidates(key), arity) else deepCandidates(predicate, arity)

// The clauses of a large [predicate] a goal with no first argument may match, if the index of the next bound
// argument that has one says
private fun Machine.deepCandidates(
    predicate: Predicate,
    arity: Int,
): IntArray? {
    if (!predicate.deep) return null
    val end = minOf(arity - 1, Predicate.MAX_DEEP_POSITION)
    var position = 1
    while (position <= end) {
        val k = keyOfArgument(position)
        if (k != 0) predicate.candidatesAt(position, k)?.let { return it }
        position++
    }
    return null
}

// The clauses [byFirst] the first argument leaves or, if they are many, those of the shortest list another argument has
private fun Machine.narrowed(
    predicate: Predicate,
    byFirst: IntArray,
    arity: Int,
): IntArray {
    if (byFirst.size < Predicate.INDEXED_FROM || !predicate.deep) return byFirst
    var best = byFirst
    val end = minOf(arity - 1, Predicate.MAX_DEEP_POSITION)
    var position = 1
    while (position <= end) {
        val k = keyOfArgument(position)
        if (k != 0) {
            val candidates = predicate.candidatesAt(position, k)
            if (candidates != null && candidates.size < best.size) best = candidates
        }
        position++
    }
    return best
}

// [tryClauses] over the clauses an index gave
private fun Machine.tryCandidates(
    table: ClauseTable,
    candidates: IntArray,
    functor: Int,
    arity: Int,
    barrier: Int,
    key: Int,
): Boolean {
    val firstKeys = table.firstKeys
    val size = candidates.size
    // (one clause: its head decides)
    if (size == 1) return entered(table, candidates[0], barrier)
    var i = 0
    if (table.innerDeep) {
        while (i < size && !matches(table.clauses[candidates[i]]!!, key, arity)) i++
    } else {
        while (i < size && !keyMatches(firstKeys[candidates[i]], key)) i++
    }
    if (i == size) return false
    val clause = table.clauses[candidates[i]]!!
    if (i + 1 < size && !commits(clause)) {
        val q = nextPosition(table, candidates, i + 1, key, arity, clause.guarded)
        if (q < size) {
            delayChoicePoint(table, candidates, functor, clause, q, arity, key)
            delayedFree = false
        }
    }
    return entered(table, candidates[i], barrier)
}

// The alternatives a builtin left of the clauses of the theory, selected now that they are tried
internal fun Machine.selectTheoryClauses(
    functor: Int,
    arity: Int,
    cp: Int,
    base: Int,
) {
    val predicate = program.theory.predicate(functor)!!
    val table = predicate.table
    if (!table.pinned) table.pinned = true
    val key = if (arity > 0) keyOfArgument(0) else 0
    cpTables[cp] = table
    cpCandidates[cp] = if (table.size >= Predicate.INDEXED_FROM) selected(predicate, key, arity) else null
    cps[base + CP_KEY] = key
}

// The next clause of the choice point on top (at [base]; [NONE]: of the one delayed) from the position [from] among its
// clauses or its candidates, the registers holding its arguments: the choice point stays for the one after it, if any,
// and goes otherwise; false when none is left
internal fun Machine.retryClauses(
    table: ClauseTable,
    candidates: IntArray?,
    from: Int,
    arity: Int,
    barrier: Int,
    key: Int,
    free: Boolean,
    base: Int,
): Boolean {
    val firstKeys = table.firstKeys
    val clause: Int
    val next: Int
    val more: Boolean
    if (candidates != null) {
        val size = candidates.size
        var p = from
        while (p < size && !keyMatches(firstKeys[candidates[p]], key)) p++
        if (p >= size) {
            dropAlternatives(base)
            return false
        }
        clause = candidates[p]
        next = nextPosition(table, candidates, p + 1, key, arity, false)
        more = next < size
    } else {
        val n = table.size
        var first = from
        while (first < n && !keyMatches(firstKeys[first], key)) first++
        if (first >= n) {
            dropAlternatives(base)
            return false
        }
        clause = first
        next = nextCandidate(table, first + 1, key, arity, free)
        more = next < n
    }
    when {
        !more -> dropAlternatives(base)
        base < 0 -> {
            delayedNext = next
            if (!delayedSaved && !table.clauses[clause]!!.guarded) saveDelayedArguments(arity)
        }
        else -> {
            truncateCalls(cps[base + CP_CALLS])
            cps[base + CP_NEXT] = next
        }
    }
    return entered(table, clause, barrier)
}

// No clause is left of the choice point at [base], or of the one delayed
private fun Machine.dropAlternatives(base: Int) {
    if (base < 0) dropDelayed() else popChoicePoint()
}

// The first clause from [from] on the keys of all its arguments do not exclude
private fun Machine.nextCandidate(
    table: ClauseTable,
    from: Int,
    key: Int,
    arity: Int,
    free: Boolean,
): Int {
    val n = table.size
    val firstKeys = table.firstKeys
    var j = from
    if (table.deep && !free) {
        while (j < n && !(keyMatches(firstKeys[j], key) && matches(table.clauses[j]!!, key, arity))) j++
    } else {
        while (j < n && !keyMatches(firstKeys[j], key)) j++
    }
    return j
}

// [nextCandidate] among the clauses an index gave: its position among them
private fun Machine.nextPosition(
    table: ClauseTable,
    candidates: IntArray,
    from: Int,
    key: Int,
    arity: Int,
    free: Boolean,
): Int {
    val size = candidates.size
    val firstKeys = table.firstKeys
    var q = from
    if (table.deep && !free) {
        while (q < size && !(keyMatches(firstKeys[candidates[q]], key) && matches(table.clauses[candidates[q]]!!, key, arity))) q++
    } else {
        while (q < size && !keyMatches(firstKeys[candidates[q]], key)) q++
    }
    return q
}

private fun Machine.matches(
    clause: ClauseCode,
    key: Int,
    arity: Int,
): Boolean {
    val keys = clause.keys
    if (arity == 0) return true
    if (!keyMatches(keys[0], key)) return false
    for (k in 1 until arity) {
        val ck = keys[k]
        if (ck != 0 && !keyMatches(ck, keyOfArgument(k))) return false
    }
    val inner = clause.innerKeys
    return inner.isEmpty() || innerMatches(inner)
}

// Whether the arguments of the first argument of the goal may match the keys of those of a clause
private fun Machine.innerMatches(inner: IntArray): Boolean {
    val d = heap.deref(regs[0])
    val base =
        when (tag(d)) {
            STR -> payload(d) + 1
            LIST -> payload(d)
            else -> return true
        }
    for (i in inner.indices) {
        val ck = inner[i]
        if (ck != 0 && !keyMatches(ck, keyOf(heap.deref(heap.cells[base + i])))) return false
    }
    return true
}

// Whether a key of a clause and one of the goal may match: equal, or either of a variable
private fun keyMatches(
    clauseKey: Int,
    goalKey: Int,
): Boolean = clauseKey == 0 || goalKey == 0 || clauseKey == goalKey
