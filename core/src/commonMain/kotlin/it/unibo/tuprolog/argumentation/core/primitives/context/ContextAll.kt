package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.Program
import it.unibo.tuprolog.argumentation.engine.vm.ProgramStore
import it.unibo.tuprolog.argumentation.engine.vm.replyWord
import it.unibo.tuprolog.solve.Signature

/**
 * context_all(+Goal, -Instances): the instances of Goal in the active branch as one list, `findall` as a single step.
 */
internal class ContextAll(
    // The context this primitive reads, for the native primitives of the other libraries (see Context.of)
    val owner: Context,
) : AbstractNativePrimitive(Signature("context_all", 2)),
    ProgramStore {
    override fun bind(program: Program) = owner.bind(program)

    override fun call(call: NativeCall): Int {
        val goal = call.arg(0)
        return call.replyWord(1, call.findall(goal, call.struct("context_check", intArrayOf(goal))))
    }
}
