package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_reset: back to a single empty branch */
internal class ContextReset(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_reset", 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        branches.reset()
        return sequenceOf(request.replyWith(true))
    }
}
