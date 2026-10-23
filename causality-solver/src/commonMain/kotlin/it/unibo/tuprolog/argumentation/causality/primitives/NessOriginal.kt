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
 * ness_original(-Intervention, +Cause, +Effect): Cause is a NESS cause of Effect as published (ICAIL 2025):
 * Intervention, removing Cause, refutes an explanation of Effect, and no part of it does. Intervention must be unbound.
 */
internal class NessOriginal(
    private val analysis: CausalityAnalysis,
) : PrimitiveWithSignature {
    override val signature = Signature("ness_original", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (intervention, cause, effect) = request.arguments
        if (intervention !is Var) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.VARIABLE, intervention)
        }
        val found = analysis.interventions(request, cause, effect).firstOrNull { analysis.nessOriginal(request, it, effect) }
        return sequenceOf(request.replyWith(found?.let { Substitution.of(intervention, it) } ?: Substitution.failed()))
    }
}
