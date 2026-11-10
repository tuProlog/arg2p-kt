package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives

import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.items
import it.unibo.tuprolog.argumentation.engine.natives.replyItems
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
import it.unibo.tuprolog.solve.Signature

/**
 * subtract(+List, +Removed, ?Rest): the elements of List identical to none of Removed (as ==), in order. Linear in both
 * lists: the elements of Removed go in a table of their hashes, when they are more than a few
 */
internal object Subtract : AbstractNativePrimitive(Signature("subtract", 3)) {
    private const val FEW = 8

    override fun call(call: NativeCall): Int {
        val items = call.items(call.arg(0)) ?: return NativeCall.FAIL
        val removed = call.items(call.arg(1)) ?: return NativeCall.FAIL
        val table = if (removed.size > FEW) Table(call, removed) else null
        val rest = IntArray(items.size)
        var n = 0
        for (item in items) {
            val gone = table?.contains(item) ?: removed.any { call.identical(item, it) }
            if (!gone) rest[n++] = item
        }
        return if (n == items.size) call.replyWord(2, call.arg(0)) else call.replyItems(2, rest.copyOf(n))
    }

    // Open addressing: the positions in [terms], from 1, by hash (the words of a kind share their low bits)
    private class Table(
        private val call: NativeCall,
        private val terms: IntArray,
    ) {
        private val slots = IntArray(terms.size.takeHighestOneBit() * 4)
        private val mask = slots.size - 1

        init {
            for (i in terms.indices) {
                val at = slot(terms[i])
                if (slots[at] == 0) slots[at] = i + 1
            }
        }

        // The slot of the element identical to [w], or the free one where it would go
        private fun slot(w: Int): Int {
            var at = (call.hashOf(w) * -1640531527).let { it xor (it ushr 15) } and mask
            while (slots[at] != 0 && !call.identical(terms[slots[at] - 1], w)) at = (at + 1) and mask
            return at
        }

        fun contains(w: Int): Boolean = slots[slot(w)] != 0
    }
}
