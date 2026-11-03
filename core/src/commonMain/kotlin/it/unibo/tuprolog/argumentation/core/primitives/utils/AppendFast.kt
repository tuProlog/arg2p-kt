package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.listArg
import it.unibo.tuprolog.argumentation.core.engine.replyItems
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/**
 * appendFast(+List1, +List2, -List): List is List1 followed by List2, in one step
 */
internal object AppendFast : AbstractNativePrimitive(Signature("appendFast", 3)) {
    override fun call(call: NativeCall): Int {
        val list1 = call.listArg(0, signature)
        val list2 = call.listArg(1, signature)
        val result = call.arg(2)
        if (call.kind(result) !=
            NativeCall.VAR
        ) {
            throw TypeError.forGoal(call.context, signature, TypeError.Expected.VARIABLE, call.term(result))
        }
        return call.replyItems(2, list1 + list2)
    }
}
