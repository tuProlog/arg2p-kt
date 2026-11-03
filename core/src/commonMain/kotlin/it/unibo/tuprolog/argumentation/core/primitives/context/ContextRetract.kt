package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_retract(+Clause): the clauses of the active branch unifying with Clause removed */
internal class ContextRetract(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_retract", 1)) {
    override fun call(call: NativeCall): Int {
        branches.selected.retractAll(call.termArg(0))
        return NativeCall.CONTINUE
    }
}
