package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
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
        val store =
            if (arity == 2) {
                branches.selected
            } else {
                branches.storeOf(call.intArg(0, signature).toInt()) ?: return NativeCall.FAIL
            }
        val goal = call.termArg(arity - 2)
        val instances = store.solve(goal).map { goal.apply(it) }.toList()
        return call.replyTerms(arity - 1, instances)
    }
}
