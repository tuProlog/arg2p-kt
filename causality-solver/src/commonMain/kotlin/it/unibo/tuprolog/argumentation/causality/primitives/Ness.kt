package it.unibo.tuprolog.argumentation.causality.primitives

import it.unibo.tuprolog.argumentation.causality.helpers.CausalityAnalysis
import it.unibo.tuprolog.argumentation.engine.vm.PrimitiveWithSignature
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * ness(-Intervention, +Cause, +Effect), and ness_original(-Intervention, +Cause, +Effect) as published (ICAIL 2025):
 * Cause is a NESS cause of Effect: Intervention, removing Cause, refutes an explanation of Effect as no part of it does
 * (see CausalityAnalysis.ness); the revised condition asks as well that an argument for Cause be IN in the grounded
 * labelling of the theory. Intervention must be unbound.
 */
internal class Ness(
    private val analysis: CausalityAnalysis,
    private val revised: Boolean,
) : PrimitiveWithSignature {
    override val signature = Signature(if (revised) "ness" else "ness_original", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (intervention, cause, effect) = request.arguments
        if (intervention !is Var) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.VARIABLE, intervention)
        }
        val found =
            if (!revised || analysis.isAccepted(request, cause)) {
                analysis.interventions(request, cause, effect).firstOrNull { analysis.ness(request, it, effect, revised) }
            } else {
                null
            }
        return sequenceOf(request.replyWith(found?.let { Substitution.of(intervention, it) } ?: Substitution.failed()))
    }
}
