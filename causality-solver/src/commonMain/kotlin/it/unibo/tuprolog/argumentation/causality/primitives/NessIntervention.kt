package it.unibo.tuprolog.argumentation.causality.primitives

import it.unibo.tuprolog.argumentation.causality.helpers.CausalityAnalysis
import it.unibo.tuprolog.argumentation.engine.vm.PrimitiveWithSignature
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * ness_intervention(+Intervention, +Effect), and ness_original_intervention(+Intervention, +Effect) as published
 * (ICAIL 2025): Intervention, a list of statements, meets the NESS condition for Effect: it refutes an explanation of
 * Effect as no part of it does (see CausalityAnalysis.ness).
 */
internal class NessIntervention(
    private val analysis: CausalityAnalysis,
    private val revised: Boolean,
) : PrimitiveWithSignature {
    override val signature = Signature(if (revised) "ness_intervention" else "ness_original_intervention", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (intervention, effect) = request.arguments
        return sequenceOf(request.replyWith(analysis.ness(request, intervention.castToList(), effect, revised)))
    }
}
