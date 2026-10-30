package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.renamedCopy
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.unify.Unificator

/**
 * subsumes(+General, +Specific): Specific is an instance of General (subsumes_term/2, which the solver lacks): a copy
 * of General unifies with Specific, the variables of Specific taken as constants. Neither is bound.
 */
internal object Subsumes : BinaryRelation.WithoutSideEffects<ExecutionContext>("subsumes") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        val frozen =
            second.apply(
                Substitution.of(
                    second.variables
                        .distinct()
                        .mapIndexed { i, v -> v to Atom.of("\$subsumed_$i") }
                        .toMap(),
                ),
            )
        return if (Unificator.default
                .mgu(
                    first.renamedCopy(),
                    frozen,
                    true,
                ).isSuccess
        ) {
            sequenceOf(Substitution.empty())
        } else {
            emptySequence()
        }
    }
}
