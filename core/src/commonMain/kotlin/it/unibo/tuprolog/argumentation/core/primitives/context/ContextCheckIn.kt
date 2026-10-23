package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.StoreClauses
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.argumentation.core.runtime.context.branchLookup
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_check(+Branch, Goal): the solutions of Goal in the given branch, as context_check/1 */
internal class ContextCheckIn(
    private val branches: ContextBranches,
) : PrimitiveWithSignature,
    DelegatedCall {
    override val signature = Signature("context_check", 2)

    // A branch that does not exist has no solutions (solved as the primitive does)
    override fun delegate(request: Solve.Request<ExecutionContext>): Delegation? {
        val branch = request.branchArgument(0)
        val store = branches.storeOf(branch) ?: return null
        return StoreClauses.delegation(store, branchLookup(branch), request.arguments[1], request.context.libraries)
    }

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        branches
            .storeOf(request.branchArgument(0))
            ?.solve(request.arguments[1])
            ?.map { request.replyWith(it) }
            ?: emptySequence()
}
