package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_absent(Goal): `\\+ context_check(Goal)` as a single step */
internal class ContextAbsent(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_absent", 1)) {
    override fun call(call: NativeCall): Int =
        call.reply(call.countSolutions(call.struct("context_check", intArrayOf(call.arg(0))), 1) == 0)
}
