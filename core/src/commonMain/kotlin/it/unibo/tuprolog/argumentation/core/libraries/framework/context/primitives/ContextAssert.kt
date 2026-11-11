package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.solve.Signature

/** contextAssert(+Clause): Clause added to the active branch */
internal class ContextAssert(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("contextAssert", 1)) {
    override fun call(call: NativeCall): Int {
        branches.selected.assertA(call.record(call.arg(0)))
        return NativeCall.CONTINUE
    }
}
