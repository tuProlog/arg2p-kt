package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.solve.Signature

/** contextAssertId(+Id): the clause with identifier Id (see contextId/2) added to the active branch, not copied */
internal class ContextAssertId(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("contextAssertId", 1)) {
    override fun call(call: NativeCall): Int {
        val id = call.arg(0)
        if (call.kind(id) != NativeCall.INT) return NativeCall.FAIL
        branches.selected.assertA(branches.record(call.intValue(id).toInt()) ?: return NativeCall.FAIL)
        return NativeCall.CONTINUE
    }
}
