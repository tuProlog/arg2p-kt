package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature

/**
 * sortDistinct(+List, ?Sorted): reverse standard order of terms, duplicates removed (deduplicate/2, then sort/2)
 */
internal object SortDistinct : AbstractNativePrimitive(Signature("sortDistinct", 2)) {
    override fun call(call: NativeCall): Int {
        val list = call.termArg(0) as? List ?: return NativeCall.FAIL
        return call.replyTerms(1, list.distinctSortedTerms().asReversed())
    }
}
