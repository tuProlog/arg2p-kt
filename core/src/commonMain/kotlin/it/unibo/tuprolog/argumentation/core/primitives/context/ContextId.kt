package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.replyUnifying
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_id(+Term, ?Id): the integer identifier of Term (see [ContextBranches.identifier]) */
internal class ContextId(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_id", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyUnifying(1, Numeric.of(branches.identifier(request.arguments[0]))))
}
