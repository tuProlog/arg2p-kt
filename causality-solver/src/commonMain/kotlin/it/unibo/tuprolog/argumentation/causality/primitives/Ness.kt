package it.unibo.tuprolog.argumentation.causality.primitives

import it.unibo.tuprolog.argumentation.causality.runtime.CausalityAnalysis
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * ness(-Intervention, +Cause, +Effect): Cause is a NESS cause of Effect (the revised condition): an argument for Cause
 * is IN in the grounded labelling of the theory, and Intervention, removing Cause, refutes an explanation of Effect
 * as no part of it does. Intervention must be unbound.
 */
internal class Ness(
    private val analysis: CausalityAnalysis,
) : PrimitiveWithSignature {
    override val signature = Signature("ness", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (intervention, cause, effect) = request.arguments
        if (intervention !is Var) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.VARIABLE, intervention)
        }
        val found =
            if (analysis.isAccepted(request, cause)) {
                analysis.interventions(request, cause, effect).firstOrNull { analysis.ness(request, it, effect) }
            } else {
                null
            }
        return sequenceOf(request.replyWith(found?.let { Substitution.of(intervention, it) } ?: Substitution.failed()))
    }
}
