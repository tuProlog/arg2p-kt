package it.unibo.tuprolog.argumentation.causality.primitives

import it.unibo.tuprolog.argumentation.causality.helpers.CausalityAnalysis
import it.unibo.tuprolog.argumentation.engine.natives.PrimitiveWithSignature
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * nessIntervention(+Intervention, +Effect), revised, and nessOriginalIntervention(+Intervention, +Effect), original:
 * Intervention, a list of statements added as axioms, meets the NESS condition for Effect, for some explanation of it
 * (see ness and CausalityAnalysis.ness).
 */
internal class NessIntervention(
    private val analysis: CausalityAnalysis,
    private val revised: Boolean,
) : PrimitiveWithSignature {
    override val signature = Signature(if (revised) "nessIntervention" else "nessOriginalIntervention", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (intervention, effect) = request.arguments
        return sequenceOf(request.replyWith(analysis.nessIntervention(request, intervention.castToList(), effect, revised)))
    }
}
