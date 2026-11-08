package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.items
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
