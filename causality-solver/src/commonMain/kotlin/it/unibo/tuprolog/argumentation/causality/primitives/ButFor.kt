package it.unibo.tuprolog.argumentation.causality.primitives

import it.unibo.tuprolog.argumentation.causality.runtime.CausalityAnalysis
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.core.List as PlList

/**
 * but_for(+Cause, +Effect): Cause is a but-for cause of Effect: an argument for Cause and one for Effect are IN in the
 * grounded labelling of the theory, and none for Effect is once the complement of Cause is added to the theory.
 */
internal class ButFor(
    private val analysis: CausalityAnalysis,
) : PrimitiveWithSignature {
    override val signature = Signature("but_for", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (cause, effect) = request.arguments
        val holds =
            analysis.isAccepted(request, cause) &&
                arg2pScope {
                    request
                        .solve("compl"(cause, X))
                        .filter { it.isYes }
                        .map { analysis.butFor(request, PlList.of(it.substitution[X]!!), effect) }
                        .firstOrNull() ?: false
                }
        return sequenceOf(request.replyWith(holds))
    }
}
