package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.retractAll
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** context_retract(+Clause): the clauses of the active branch unifying with Clause removed */
internal class ContextRetract(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_retract", 1)) {
    override fun call(call: NativeCall): Int {
        call.retractAll(branches.selected, call.arg(0))
        return NativeCall.CONTINUE
    }
}
