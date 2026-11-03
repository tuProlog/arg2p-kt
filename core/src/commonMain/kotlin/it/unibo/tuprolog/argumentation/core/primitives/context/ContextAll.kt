package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/**
 * context_all(+Goal, -Instances): the instances of Goal in the active branch as one list, `findall` as a single step.
 */
internal class ContextAll(
    arity: Int,
    private val branches: ContextBranches,
    // The context this primitive reads, for the native primitives of the other libraries (see Context.of)
    val owner: Context,
) : AbstractNativePrimitive(Signature("context_all", arity)) {
    override fun call(call: NativeCall): Int {
        val arity = call.arity
        val goal = call.arg(arity - 2)
        val lookup =
            if (arity == 2) {
                call.struct("context_check", intArrayOf(goal))
            } else {
                val branch = call.intArg(0, signature).toInt()
                if (branches.storeOf(branch) == null) return NativeCall.FAIL
                call.struct("context_check", intArrayOf(call.int(branch.toLong()), goal))
            }
        val instances = call.findall(goal, lookup)
        return call.replyWord(arity - 1, instances)
    }
}
