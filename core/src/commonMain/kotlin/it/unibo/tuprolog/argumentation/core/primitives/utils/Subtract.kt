package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature

/**
 * subtract(+List, +Removed, ?Rest): the elements of List not in Removed, in order (elements compared as ground terms),
 * without the recursion of a Prolog subtract (whose frames would all keep the rest of the list)
 */
internal object Subtract : AbstractNativePrimitive(Signature("subtract", 3)) {
    override fun call(call: NativeCall): Int {
        val first = call.termArg(0) as? List ?: return NativeCall.FAIL
        val second = call.termArg(1) as? List ?: return NativeCall.FAIL
        val removed = second.toList().toSet()
        return call.replyTerms(2, first.toList().filter { it !in removed })
    }
}
