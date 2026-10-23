package it.unibo.tuprolog.argumentation.causality.primitives

import it.unibo.tuprolog.argumentation.causality.runtime.CausalityAnalysis
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * ness_intervention(+Intervention, +Effect): Intervention, a list of statements, meets the revised NESS condition for
 * Effect: it refutes an explanation of Effect as no part of it does.
 */
internal class NessIntervention(
    private val analysis: CausalityAnalysis,
) : PrimitiveWithSignature {
    override val signature = Signature("ness_intervention", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (intervention, effect) = request.arguments
        return sequenceOf(request.replyWith(analysis.ness(request, intervention.castToList(), effect)))
    }
}
