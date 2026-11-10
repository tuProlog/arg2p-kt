package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.sortedTerms
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.items
import it.unibo.tuprolog.argumentation.engine.natives.replyItems
import it.unibo.tuprolog.argumentation.engine.natives.replyTerms
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature

/**
 * sortDescending(+List, ?Sorted): the reverse of the standard order of terms, duplicates kept (as the former quicksort)
 */
internal object Sort : AbstractNativePrimitive(Signature("sortDescending", 2)) {
    override fun call(call: NativeCall): Int {
        call.items(call.arg(0))?.let { items -> call.sorted(items)?.let { return call.replyItems(1, it.reversedArray()) } }
        val list = call.termArg(0) as? List ?: return NativeCall.FAIL
        return call.replyTerms(1, list.sortedTerms().asReversed())
    }
}
