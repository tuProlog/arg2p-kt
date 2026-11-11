package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.GoalWrapper
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.natives.items
import it.unibo.tuprolog.argumentation.engine.natives.words
import it.unibo.tuprolog.argumentation.engine.store.ClauseStore
import it.unibo.tuprolog.argumentation.engine.store.Record
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word

// A lookup in a store with clauses left to try: the goals of their bodies wrapped; for `clause(Head, Body)`, its
// Body in the register after the arguments of Head, unified with the stored body
internal class StoredCall(
    val candidates: ClauseStore.Candidates,
    val wrap: GoalWrapper?,
    val asClause: Boolean,
)

/**
 * The lookups of the stores on a machine: a goal resolved with the records stored for it, their heads unified against the
 * arguments in the registers, in place.
 */
internal class StoreLookup(
    private val m: Machine,
) {
    // (the heap of the machine, held: one load less on each cell read)
    private val heap = m.heap

    /** Enters the goal [goal] resolved with the clauses of [store] for it: their heads unified in place */
    fun storeGoal(
        goal: Int,
        store: ClauseStore,
        wrap: GoalWrapper?,
    ): Int {
        store.bind(m.program)
        val g = heap.deref(goal)
        val candidates = store.candidates(heap.cells, g)
        if (candidates.size == 0) return FAIL
        return tryStored(StoredCall(candidates, wrap, false), candidates.size - 1, loadArguments(g), Machine.NONE)
    }

    /** Answers `clause(Head, Body)`, for a callable [head]: the clauses of [store] whose head and body unify with them */
    fun storeClause(
        head: Int,
        body: Int,
        store: ClauseStore,
    ): Int {
        store.bind(m.program)
        val g = heap.deref(head)
        val candidates = store.candidates(heap.cells, g)
        if (candidates.size == 0) return FAIL
        val arity = loadArguments(g)
        if (arity + 1 > m.regs.size) m.regs = m.regs.copyOf((arity + 1) * 2)
        m.regs[arity] = body
        return tryStored(StoredCall(candidates, null, true), candidates.size - 1, arity + 1, Machine.NONE)
    }

    /**
     * Whether a fact of [store] unifies with [goal] (or, with [body], whether a clause does with `clause(goal, body)`),
     * nothing left bound; null when a rule may answer, whose body is to be solved
     */
    fun storeHolds(
        goal: Int,
        body: Int?,
        store: ClauseStore,
    ): Boolean? {
        store.bind(m.program)
        val g = heap.deref(goal)
        val candidates = store.candidates(heap.cells, g)
        val items = candidates.items
        val arity = loadArguments(g)
        for (i in candidates.size - 1 downTo 0) {
            val record = items[i]!!.record
            if (!headMayMatch(record, arity)) continue
            if (body == null && record.isRule) return null
            if (recordUnifies(record, arity) { body == null || heap.unify(body, copyRecord(record.words, record.body)) }) return true
        }
        return false
    }

    // Whether the head of [record] unifies with the arguments in the registers and [then] holds, nothing left bound
    private inline fun recordUnifies(
        record: Record,
        arity: Int,
        then: () -> Boolean,
    ): Boolean =
        heap.tentatively {
            enterRecord(record)
            unifyHead(record, arity) && then()
        }

    /** The facts of [store] that unify with [goal], newest first, nothing left bound and the registers as they were */
    fun storeMatching(
        goal: Int,
        store: ClauseStore,
    ): List<ClauseStore.Stored> {
        store.bind(m.program)
        val g = heap.deref(goal)
        val candidates = store.candidates(heap.cells, g)
        val items = candidates.items
        val arity =
            when (tag(g)) {
                STR -> m.program.arity(heap.cells[payload(g)])
                LIST -> 2
                else -> 0
            }
        val saved = m.regs.copyOf(arity)
        loadArguments(g)
        val matching = ArrayList<ClauseStore.Stored>()
        for (i in candidates.size - 1 downTo 0) {
            val stored = items[i]!!
            val record = stored.record
            if (record.isRule || !headMayMatch(record, arity)) continue
            if (recordUnifies(record, arity) { true }) matching.add(stored)
        }
        saved.copyInto(m.regs)
        return matching
    }

    // Tries the clauses of [call] from position [from] down: past the heads that cannot match, a choice point left if
    // another may. [index] is that of the call among [calls], once it has one
    fun tryStored(
        call: StoredCall,
        from: Int,
        arity: Int,
        index: Int,
    ): Int {
        val items = call.candidates.items
        val headArity = if (call.asClause) arity - 1 else arity
        var i = from
        while (i >= 0 && !headMayMatch(items[i]!!.record, headArity)) i--
        if (i < 0) return FAIL
        var next = i - 1
        while (next >= 0 && !headMayMatch(items[next]!!.record, headArity)) next--
        if (next >= 0) {
            val at =
                if (index != Machine.NONE) {
                    index
                } else {
                    m.calls.add(call)
                    m.calls.size - 1
                }
            m.pushChoicePoint(Machine.STORED, at, arity, 0, m.continuation, next)
        }
        val record = items[i]!!.record
        enterRecord(record)
        if (!unifyHead(record, headArity)) return FAIL
        if (call.asClause) return if (heap.unify(m.regs[headArity], copyRecord(record.words, record.body))) CONTINUE else FAIL
        if (!record.isRule) return CONTINUE
        val body = copyRecord(record.words, record.body)
        val wrap = call.wrap
        m.cutBarrier = m.cpTop
        return m.callTerm(if (wrap == null) body else wrapped(body, wrap))
    }

    // ---- a record against the heap, in place: nothing is copied but what a variable of the heap is bound to

    // The variables of the record at hand, by their relative addresses: their words on the heap, for this stamp
    private var recordVariables = IntArray(64)
    private var recordStamps = IntArray(64)
    private var recordStamp = 0

    private fun enterRecord(record: Record) {
        val n = record.words.size
        if (n > recordStamps.size) {
            recordVariables = IntArray(n * 2)
            recordStamps = IntArray(n * 2)
            recordStamp = 0
        }
        recordStamp++
    }

    // The arguments of the head of [record] against the registers
    private fun unifyHead(
        record: Record,
        arity: Int,
    ): Boolean {
        val words = record.words
        val head = record.head
        val offset = if (tag(head) == LIST) payload(head) else payload(head) + 1
        for (i in 0 until arity) if (!unifyRecord(words, words[offset + i], m.regs[i])) return false
        return true
    }

    // Whether the head of [record] may unify with the arguments in the registers: all that binds nothing told apart
    private fun headMayMatch(
        record: Record,
        arity: Int,
    ): Boolean {
        val words = record.words
        val head = record.head
        val offset = if (tag(head) == LIST) payload(head) else payload(head) + 1
        for (i in 0 until arity) if (!mayMatch(words, words[offset + i], m.regs[i])) return false
        return true
    }

    private fun mayMatch(
        words: IntArray,
        r0: Int,
        w0: Int,
    ): Boolean {
        var r = r0
        var w = w0
        while (true) {
            val t = tag(r)
            if (t == REF) return true
            val d = heap.deref(w)
            val td = tag(d)
            if (td == REF) return true
            if (t != td) return false
            when (t) {
                STR -> {
                    val a = payload(r)
                    val b = payload(d)
                    val f = words[a]
                    if (f != heap.cells[b]) return false
                    val n = m.program.arity(f)
                    for (i in 1 until n) if (!mayMatch(words, words[a + i], heap.cells[b + i])) return false
                    r = words[a + n]
                    w = heap.cells[b + n]
                }
                LIST -> {
                    val a = payload(r)
                    val b = payload(d)
                    if (!mayMatch(words, words[a], heap.cells[b])) return false
                    r = words[a + 1]
                    w = heap.cells[b + 1]
                }
                else -> return r == d
            }
        }
    }

    private fun unifyRecord(
        words: IntArray,
        r0: Int,
        w0: Int,
    ): Boolean {
        var r = r0
        var w = w0
        while (true) {
            when (tag(r)) {
                REF -> {
                    val a = payload(r)
                    if (recordStamps[a] == recordStamp) return heap.unify(recordVariables[a], w)
                    recordStamps[a] = recordStamp
                    recordVariables[a] = w
                    return true
                }
                STR -> {
                    val d = heap.deref(w)
                    when (tag(d)) {
                        REF -> {
                            heap.bind(payload(d), copyRecord(words, r))
                            return true
                        }
                        STR -> {
                            val a = payload(r)
                            val b = payload(d)
                            val f = words[a]
                            if (f != heap.cells[b]) return false
                            val n = m.program.arity(f)
                            for (i in 1 until n) if (!unifyRecord(words, words[a + i], heap.cells[b + i])) return false
                            r = words[a + n]
                            w = heap.cells[b + n]
                        }
                        else -> return false
                    }
                }
                LIST -> {
                    val d = heap.deref(w)
                    when (tag(d)) {
                        REF -> {
                            heap.bind(payload(d), copyRecord(words, r))
                            return true
                        }
                        LIST -> {
                            val a = payload(r)
                            val b = payload(d)
                            if (!unifyRecord(words, words[a], heap.cells[b])) return false
                            r = words[a + 1]
                            w = heap.cells[b + 1]
                        }
                        else -> return false
                    }
                }
                else -> {
                    val d = heap.deref(w)
                    if (d == r) return true
                    if (tag(d) != REF) return false
                    heap.bind(payload(d), r)
                    return true
                }
            }
        }
    }

    // The term [r] of a record on the heap, its variables those met so far or fresh ones
    private fun copyRecord(
        words: IntArray,
        r: Int,
    ): Int =
        when (tag(r)) {
            REF -> {
                val a = payload(r)
                if (recordStamps[a] == recordStamp) {
                    recordVariables[a]
                } else {
                    val v = ref(heap.newVariable(0))
                    recordStamps[a] = recordStamp
                    recordVariables[a] = v
                    v
                }
            }
            STR -> {
                val a = payload(r)
                val f = words[a]
                val n = m.program.arity(f)
                val h = heap.alloc(n + 1)
                heap.cells[h] = f
                for (i in 1..n) {
                    val argument = copyRecord(words, words[a + i])
                    heap.cells[h + i] = argument
                }
                word(STR, h)
            }
            LIST -> {
                val first = heap.alloc(2)
                var cell = first
                var a = payload(r)
                while (true) {
                    val item = copyRecord(words, words[a])
                    heap.cells[cell] = item
                    val tail = words[a + 1]
                    if (tag(tail) == LIST) {
                        val next = heap.alloc(2)
                        heap.cells[cell + 1] = word(LIST, next)
                        cell = next
                        a = payload(tail)
                    } else {
                        val last = copyRecord(words, tail)
                        heap.cells[cell + 1] = last
                        break
                    }
                }
                word(LIST, first)
            }
            else -> r
        }

    // The arguments of the goal [g] (dereferenced) in the registers; its arity
    private fun loadArguments(g: Int): Int =
        when (tag(g)) {
            STR -> {
                val a = payload(g)
                val n = m.program.arity(heap.cells[a])
                if (n > m.regs.size) m.regs = m.regs.copyOf(n * 2)
                for (i in 0 until n) m.regs[i] = heap.cells[a + 1 + i]
                n
            }
            LIST -> {
                val a = payload(g)
                m.regs[0] = heap.cells[a]
                m.regs[1] = heap.cells[a + 1]
                2
            }
            else -> 0
        }

    // The body [w] with each of its goals wrapped by [wrap]: a conjunction rebuilt, `true` left alone
    private fun wrapped(
        w: Int,
        wrap: GoalWrapper,
    ): Int {
        val d = heap.deref(w)
        if (tag(d) == STR && heap.cells[payload(d)] == m.wComma) {
            val a = payload(d)
            val left = wrapped(heap.cells[a + 1], wrap)
            val right = wrapped(heap.cells[a + 2], wrap)
            val h = heap.alloc(3)
            heap.cells[h] = m.wComma
            heap.cells[h + 1] = left
            heap.cells[h + 2] = right
            return word(STR, h)
        }
        if (d == m.program.trueWord) return d
        return wrap.wrap(m.face, d)
    }
}
