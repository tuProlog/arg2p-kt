package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** context_reset: back to a single empty branch */
internal class ContextReset(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_reset", 0)) {
    override fun call(call: NativeCall): Int {
        branches.reset()
        return NativeCall.CONTINUE
    }
}
