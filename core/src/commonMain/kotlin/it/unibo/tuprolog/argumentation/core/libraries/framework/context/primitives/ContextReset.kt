package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.solve.Signature

/** context_reset: back to a single empty branch, a solve starting: [onReset] told, with the version of the theory */
internal class ContextReset(
    private val branches: ContextBranches,
    private val onReset: (Long?) -> Unit,
) : AbstractNativePrimitive(Signature("context_reset", 0)) {
    override fun call(call: NativeCall): Int {
        branches.reset()
        onReset(call.theoryVersion)
        return NativeCall.CONTINUE
    }
}
