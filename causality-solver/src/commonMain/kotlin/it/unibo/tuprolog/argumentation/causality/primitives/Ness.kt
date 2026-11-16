package it.unibo.tuprolog.argumentation.causality.primitives

import it.unibo.tuprolog.argumentation.causality.helpers.CausalityAnalysis
import it.unibo.tuprolog.argumentation.engine.natives.PrimitiveWithSignature
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * ness(-Intervention, +Cause, +Effect), revised (ICAIL 2026), and nessOriginal(-Intervention, +Cause, +Effect), original
 * (ICAIL 2025, Def. 4.3): Cause is a NESS cause of Effect, for some explanation of a justified argument for Effect (a
 * subset-minimal strongly admissible set containing it, Def. 3.3) and Intervention, a set of axioms containing the
 * complement of Cause. Original: Intervention removes the explanation from the grounded extension, and no proper part
 * of it does. Revised: an argument for Cause is IN, and Intervention gives an argument IN or UND attacking a member of
 * the explanation that no proper part of it gives (see CausalityAnalysis.ness). Intervention must be unbound.
 */
internal class Ness(
    private val analysis: CausalityAnalysis,
    private val revised: Boolean,
) : PrimitiveWithSignature {
    override val signature = Signature(if (revised) "ness" else "nessOriginal", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (intervention, cause, effect) = request.arguments
        if (intervention !is Var) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.VARIABLE, intervention)
        }
        val found = analysis.ness(request, cause, effect, revised)
        return sequenceOf(request.replyWith(found?.let { Substitution.of(intervention, it) } ?: Substitution.failed()))
    }
}
