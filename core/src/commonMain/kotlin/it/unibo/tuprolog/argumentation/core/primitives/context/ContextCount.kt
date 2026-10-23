package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.replyUnifying
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * context_count(+Goal, +Limit, -Count): the number of solutions of Goal in the active branch, counted up to Limit
 * (an integer, or anything else for no limit): enough to compare how selective goals are, without visiting all
 * the solutions of the least selective ones
 */
internal class ContextCount(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_count", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val limit =
            request.arguments[1]
                .asInteger()
                ?.intValue
                ?.toInt() ?: Int.MAX_VALUE
        val count =
            branches.selected
                .solve(request.arguments[0])
                .take(limit)
                .count()
        return sequenceOf(request.replyUnifying(2, Numeric.of(count)))
    }
}
