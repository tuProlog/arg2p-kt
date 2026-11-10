package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.matching
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.items
import it.unibo.tuprolog.solve.Signature

/** context_retract_all(+Clauses): the clauses of the active branch unifying with one of Clauses removed, all at once */
internal class ContextRetractAll(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_retract_all", 1)) {
    override fun call(call: NativeCall): Int {
        val patterns = call.items(call.arg(0)) ?: return NativeCall.FAIL
        val store = branches.selected
        store.removeAll(patterns.flatMap { call.matching(store, it) }.distinct())
        return NativeCall.CONTINUE
    }
}
