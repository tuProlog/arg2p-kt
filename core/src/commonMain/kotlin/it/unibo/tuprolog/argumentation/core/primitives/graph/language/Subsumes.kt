package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.subsumes
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * subsumes(+General, +Specific): Specific is an instance of General (subsumes_term/2, which the solver lacks): General
 * unifies with Specific, the variables of Specific taken as constants (see Term.subsumes). Neither is bound.
 */
internal object Subsumes : BinaryRelation.WithoutSideEffects<ExecutionContext>("subsumes") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (first.subsumes(second)) sequenceOf(Substitution.empty()) else emptySequence()
}
