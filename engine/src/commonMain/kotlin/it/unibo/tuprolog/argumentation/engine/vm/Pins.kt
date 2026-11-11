package it.unibo.tuprolog.argumentation.engine.vm

import kotlin.jvm.JvmField

/**
 * The words a machine keeps across a nested run (pinned): the variables of a query, the arguments of a native call and the
 * template and the result of a findall, roots of the collector, which moves them. A pin is a position, valid until the pins
 * are cut back below it ([truncate]); a choice point keeps the top, backtracking cuts back to it.
 */
internal class Pins : HeapCollector.Roots {
    @JvmField var words = IntArray(16)

    @JvmField var top = 0

    /** [w] pinned: its position */
    fun pin(w: Int): Int {
        if (top == words.size) words = words.copyOf(top * 2)
        words[top] = w
        return top++
    }

    /** The pins from [n] on dropped */
    fun truncate(n: Int) {
        top = n
    }

    override fun markRoots(c: HeapCollector) {
        for (i in 0 until top) c.markWord(words[i])
    }

    override fun relocateRoots(c: HeapCollector) {
        for (i in 0 until top) words[i] = c.relocated(words[i])
    }
}
