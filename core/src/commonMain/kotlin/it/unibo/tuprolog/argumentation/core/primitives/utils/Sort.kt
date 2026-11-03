package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature

/**
 * sortDescending(+List, ?Sorted): the reverse of the standard order of terms, duplicates kept (as the former quicksort)
 */
internal object Sort : AbstractNativePrimitive(Signature("sortDescending", 2)) {
    override fun call(call: NativeCall): Int {
        val list = call.termArg(0) as? List ?: return NativeCall.FAIL
        return call.replyTerms(1, list.sortedTerms().asReversed())
    }
}
