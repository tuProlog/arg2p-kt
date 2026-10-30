package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.unify.Unificator

/**
 * occursFreeUnify(?A, ?B): A and B unify with the occurs check, so that no cyclic term is built (as a rule
 * `p(X, f(X))` would build with `p(Y, Y)`), which the predicates walking the terms afterwards would never leave
 */
internal object OccursFreeUnify : BinaryRelation.WithoutSideEffects<ExecutionContext>("occursFreeUnify") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = Unificator.default.mgu(first, second, true).let { if (it.isSuccess) sequenceOf(it) else emptySequence() }
}
