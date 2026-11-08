package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag

/** What the standard predicates and the instructions ask of terms on the heap of a machine, nothing bound */
internal object HeapTerms {
    /** Whether no variable is in the term [w] */
    fun ground(
        m: Machine,
        w: Int,
    ): Boolean {
        var d = m.deref(w)
        while (true) {
            when (tag(d)) {
                REF -> return false
                STR -> {
                    val a = payload(d)
                    val n = m.program.arity(m.heap[a])
                    for (i in 1 until n) if (!ground(m, m.heap[a + i])) return false
                    d = m.deref(m.heap[a + n])
                }
                LIST -> {
                    val a = payload(d)
                    if (!ground(m, m.heap[a])) return false
                    d = m.deref(m.heap[a + 1])
                }
                else -> return true
            }
        }
    }

    /** Whether the terms are identical, as `==`: the same variables, not bound */
    fun identical(
        m: Machine,
        x0: Int,
        y0: Int,
    ): Boolean {
        var x = m.deref(x0)
        var y = m.deref(y0)
        while (true) {
            if (x == y) return true
            val t = tag(x)
            if (t != tag(y)) return false
            when (t) {
                REF -> return m.sameVariable(payload(x), payload(y))
                STR -> {
                    val a = payload(x)
                    val b = payload(y)
                    val f = m.heap[a]
                    if (f != m.heap[b]) return false
                    val n = m.program.arity(f)
                    for (i in 1 until n) if (!identical(m, m.heap[a + i], m.heap[b + i])) return false
                    x = m.deref(m.heap[a + n])
                    y = m.deref(m.heap[b + n])
                }
                LIST -> {
                    val a = payload(x)
                    val b = payload(y)
                    if (!identical(m, m.heap[a], m.heap[b])) return false
                    x = m.deref(m.heap[a + 1])
                    y = m.deref(m.heap[b + 1])
                }
                else -> return false
            }
        }
    }

    /** A hash of the term, the same for those [identical]: all the variables count alike, two cells may be the same one */
    fun hash(
        m: Machine,
        w0: Int,
        seed: Int,
    ): Int {
        var hash = seed
        var w = m.deref(w0)
        while (true) {
            when (tag(w)) {
                REF -> return 31 * hash
                STR -> {
                    val a = payload(w)
                    val f = m.heap[a]
                    hash = 31 * hash + f
                    val n = m.program.arity(f)
                    for (i in 1 until n) hash = hash(m, m.heap[a + i], hash)
                    w = m.deref(m.heap[a + n])
                }
                LIST -> {
                    val a = payload(w)
                    hash = hash(m, m.heap[a], 31 * hash + 1)
                    w = m.deref(m.heap[a + 1])
                }
                else -> return 31 * hash + w
            }
        }
    }
}
