package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.argumentation.core.runtime.context.replyUnifying
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * context_all(+Goal, -Instances): the instances of Goal in the active branch as one list, `findall` as a single step.
 * context_all(+Branch, +Goal, -Instances): the same in the given branch.
 */
internal class ContextAll(
    arity: Int,
    private val branches: ContextBranches,
    // The context this primitive reads, for the native primitives of the other libraries (see Context.of)
    val owner: Context,
) : PrimitiveWithSignature {
    override val signature = Signature("context_all", arity)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val store =
            if (request.arguments.size == 2) {
                branches.selected
            } else {
                branches.storeOf(request.branchArgument(0)) ?: return sequenceOf(request.replyFail())
            }
        val goal = request.arguments[request.arguments.size - 2]
        val instances = store.solve(goal).map { goal.apply(it) }.toList()
        return sequenceOf(request.replyUnifying(request.arguments.size - 1, List.of(instances)))
    }
}
