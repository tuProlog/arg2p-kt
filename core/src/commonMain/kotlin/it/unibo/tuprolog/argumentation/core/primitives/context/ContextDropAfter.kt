package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_drop_after(+Branch, +Keep): drops the branches created after Branch, except those in Keep and the active one */
internal class ContextDropAfter(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_drop_after", 2)) {
    override fun call(call: NativeCall): Int {
        branches.dropAfter(call.intArg(0, signature).toInt(), call.branchesArg(1, signature))
        return NativeCall.CONTINUE
    }
}
