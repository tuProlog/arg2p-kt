package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_drop(+Branch): frees a branch no longer needed, fails on the active one */
internal class ContextDrop(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_drop", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(if (branches.drop(request.branchArgument(0))) request.replyWith(true) else request.replyFail())
}
