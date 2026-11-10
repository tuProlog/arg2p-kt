package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.absentFrom
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.reply
import it.unibo.tuprolog.solve.Signature

/** context_absent(Goal): `\\+ context_check(Goal)` as a single step */
internal class ContextAbsent(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_absent", 1)) {
    override fun call(call: NativeCall): Int {
        val goal = call.arg(0)
        return call.reply(
            call.absentFrom(branches.selected, goal) ?: (call.countSolutions(call.struct("context_check", intArrayOf(goal)), 1) == 0),
        )
    }
}
