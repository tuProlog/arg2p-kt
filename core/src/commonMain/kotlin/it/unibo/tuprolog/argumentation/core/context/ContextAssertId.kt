package it.unibo.tuprolog.argumentation.core.context

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** context_assert_id(+Id): the clause with identifier Id (see context_id/2) added to the active branch, not copied */
internal class ContextAssertId(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_assert_id", 1)) {
    override fun call(call: NativeCall): Int {
        val id = call.arg(0)
        if (call.kind(id) != NativeCall.INT) return NativeCall.FAIL
        branches.selected.assertA(branches.record(call.intValue(id).toInt()) ?: return NativeCall.FAIL)
        return NativeCall.CONTINUE
    }
}
