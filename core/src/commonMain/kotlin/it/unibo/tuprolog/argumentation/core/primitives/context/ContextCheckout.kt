package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_checkout(+Branch): Branch made active; a branch that does not exist (never created, or dropped) is not */
internal class ContextCheckout(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_checkout", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(if (branches.checkout(request.branchArgument(0))) request.replyWith(true) else request.replyFail())
}
