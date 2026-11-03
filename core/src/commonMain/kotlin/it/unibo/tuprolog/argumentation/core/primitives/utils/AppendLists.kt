package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.items
import it.unibo.tuprolog.argumentation.core.engine.listArg
import it.unibo.tuprolog.argumentation.core.engine.replyItems
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/** appendLists(Lists, Result): Result is the concatenation of the lists in Lists */
internal object AppendLists : AbstractNativePrimitive(Signature("appendLists", 2)) {
    override fun call(call: NativeCall): Int {
        val items =
            call.listArg(0, signature).flatMap { list ->
                (
                    call.items(
                        list,
                    ) ?: throw TypeError.forGoal(call.context, signature, TypeError.Expected.LIST, call.term(list))
                ).asIterable()
            }
        return call.replyItems(1, items.toIntArray())
    }
}
