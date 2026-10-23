package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.replyUnifying
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_last(-Branch): the most recently created branch */
internal class ContextLast(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_last", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyUnifying(0, Integer.of(branches.last)))
}
