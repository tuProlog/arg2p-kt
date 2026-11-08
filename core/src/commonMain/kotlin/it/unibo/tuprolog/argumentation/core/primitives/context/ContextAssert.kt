package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** context_assert(+Clause): Clause added to the active branch */
internal class ContextAssert(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_assert", 1)) {
    override fun call(call: NativeCall): Int {
        branches.selected.assertA(call.record(call.arg(0)))
        return NativeCall.CONTINUE
    }
}
