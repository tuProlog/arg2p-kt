package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall.Companion.FAIL
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
    /** Enters the goal [goal] resolved with the clauses of [store] for it: their heads unified in place */
    fun storeGoal(
        goal: Int,
        store: ClauseStore,
        wrap: GoalWrapper?,
    ): Int {
        store.bind(m.program)
        val g = m.deref(goal)
        val candidates = store.candidates(m.heap, g)
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
        val g = m.deref(head)
        val candidates = store.candidates(m.heap, g)
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
        val g = m.deref(goal)
        val candidates = store.candidates(m.heap, g)
        val items = candidates.items
        val arity = loadArguments(g)
        for (i in candidates.size - 1 downTo 0) {
            val record = items[i]!!.record
            if (!headMayMatch(record, arity)) continue
            if (body == null && record.isRule) return null
            if (recordUnifies(record, arity) { body == null || m.unify(body, copyRecord(record.words, record.body)) }) return true
        }
        return false
    }

    // Whether the head of [record] unifies with the arguments in the registers and [then] holds, nothing left bound
    private inline fun recordUnifies(
        record: Record,
        arity: Int,
        then: () -> Boolean,
    ): Boolean {
        val savedTop = m.top
        val savedBarrier = m.heapBarrier
        val mark = m.trailTop
        m.heapBarrier = Int.MAX_VALUE
        enterRecord(record)
        val holds = unifyHead(record, arity) && then()
        m.undoTrail(mark)
        m.heapBarrier = savedBarrier
        m.top = savedTop
        return holds
    }

    /** The facts of [store] that unify with [goal], newest first, nothing left bound and the registers as they were */
    fun storeMatching(
        goal: Int,
        store: ClauseStore,
    ): List<ClauseStore.Stored> {
        store.bind(m.program)
        val g = m.deref(goal)
        val candidates = store.candidates(m.heap, g)
        val items = candidates.items
        val arity =
            when (tag(g)) {
                STR -> m.program.arity(m.heap[payload(g)])
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
        if (call.asClause) return if (m.unify(m.regs[headArity], copyRecord(record.words, record.body))) CONTINUE else FAIL
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
            val d = m.deref(w)
            val td = tag(d)
            if (td == REF) return true
            if (t != td) return false
            when (t) {
                STR -> {
                    val a = payload(r)
                    val b = payload(d)
                    val f = words[a]
                    if (f != m.heap[b]) return false
                    val n = m.program.arity(f)
                    for (i in 1 until n) if (!mayMatch(words, words[a + i], m.heap[b + i])) return false
                    r = words[a + n]
                    w = m.heap[b + n]
                }
                LIST -> {
                    val a = payload(r)
                    val b = payload(d)
                    if (!mayMatch(words, words[a], m.heap[b])) return false
                    r = words[a + 1]
                    w = m.heap[b + 1]
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
                    if (recordStamps[a] == recordStamp) return m.unify(recordVariables[a], w)
                    recordStamps[a] = recordStamp
                    recordVariables[a] = w
                    return true
                }
                STR -> {
                    val d = m.deref(w)
                    when (tag(d)) {
                        REF -> {
                            m.bind(payload(d), copyRecord(words, r))
                            return true
                        }
                        STR -> {
                            val a = payload(r)
                            val b = payload(d)
                            val f = words[a]
                            if (f != m.heap[b]) return false
                            val n = m.program.arity(f)
                            for (i in 1 until n) if (!unifyRecord(words, words[a + i], m.heap[b + i])) return false
                            r = words[a + n]
                            w = m.heap[b + n]
                        }
                        else -> return false
                    }
                }
                LIST -> {
                    val d = m.deref(w)
                    when (tag(d)) {
                        REF -> {
                            m.bind(payload(d), copyRecord(words, r))
                            return true
                        }
                        LIST -> {
                            val a = payload(r)
                            val b = payload(d)
                            if (!unifyRecord(words, words[a], m.heap[b])) return false
                            r = words[a + 1]
                            w = m.heap[b + 1]
                        }
                        else -> return false
                    }
                }
                else -> {
                    val d = m.deref(w)
                    if (d == r) return true
                    if (tag(d) != REF) return false
                    m.bind(payload(d), r)
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
                    val v = ref(m.newVariable(0))
                    recordStamps[a] = recordStamp
                    recordVariables[a] = v
                    v
                }
            }
            STR -> {
                val a = payload(r)
                val f = words[a]
                val n = m.program.arity(f)
                val h = m.alloc(n + 1)
                m.heap[h] = f
                for (i in 1..n) {
                    val argument = copyRecord(words, words[a + i])
                    m.heap[h + i] = argument
                }
                word(STR, h)
            }
            LIST -> {
                val first = m.alloc(2)
                var cell = first
                var a = payload(r)
                while (true) {
                    val item = copyRecord(words, words[a])
                    m.heap[cell] = item
                    val tail = words[a + 1]
                    if (tag(tail) == LIST) {
                        val next = m.alloc(2)
                        m.heap[cell + 1] = word(LIST, next)
                        cell = next
                        a = payload(tail)
                    } else {
                        val last = copyRecord(words, tail)
                        m.heap[cell + 1] = last
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
                val n = m.program.arity(m.heap[a])
                if (n > m.regs.size) m.regs = m.regs.copyOf(n * 2)
                for (i in 0 until n) m.regs[i] = m.heap[a + 1 + i]
                n
            }
            LIST -> {
                val a = payload(g)
                m.regs[0] = m.heap[a]
                m.regs[1] = m.heap[a + 1]
                2
            }
            else -> 0
        }

    // The body [w] with each of its goals wrapped by [wrap]: a conjunction rebuilt, `true` left alone
    private fun wrapped(
        w: Int,
        wrap: GoalWrapper,
    ): Int {
        val d = m.deref(w)
        if (tag(d) == STR && m.heap[payload(d)] == m.wComma) {
            val a = payload(d)
            val left = wrapped(m.heap[a + 1], wrap)
            val right = wrapped(m.heap[a + 2], wrap)
            val h = m.alloc(3)
            m.heap[h] = m.wComma
            m.heap[h + 1] = left
            m.heap[h + 2] = right
            return word(STR, h)
        }
        if (d == m.program.trueWord) return d
        return wrap.wrap(m, d)
    }
}
