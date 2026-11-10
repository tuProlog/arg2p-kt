package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.distinctSortedTerms
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.items
import it.unibo.tuprolog.argumentation.engine.natives.replyItems
import it.unibo.tuprolog.argumentation.engine.natives.replyTerms
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature

/**
 * deduplicate(+List, ?Deduplicated): standard order of terms, duplicates removed (as setof/3)
 */
internal object Deduplicate : AbstractNativePrimitive(Signature("deduplicate", 2)) {
    override fun call(call: NativeCall): Int {
        call.items(call.arg(0))?.let { items -> call.sortedDistinct(items)?.let { return call.replyItems(1, it) } }
        val list = call.termArg(0) as? List ?: return NativeCall.FAIL
        return call.replyTerms(1, list.distinctSortedTerms())
    }
}
