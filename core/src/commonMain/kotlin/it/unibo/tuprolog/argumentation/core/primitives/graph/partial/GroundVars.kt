package it.unibo.tuprolog.argumentation.core.primitives.graph.partial

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * groundVars(+Term, ?Grounded): Term with each of its variables replaced by an atom of its own (`$var_0`, ...):
 * a term (with none of these atoms) unifies with Grounded if and only if it generalises Term
 */
object GroundVars : BinaryRelation.WithoutSideEffects<ExecutionContext>("groundVars") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        Arg2pUnificator
            .mgu(
                second,
                first.apply(
                    Substitution.of(
                        first.variables.distinct().withIndex().associate { (index, variable) ->
                            variable to
                                Atom.of("\$var_$index")
                        },
                    ),
                ),
            ).let { if (it.isSuccess) sequenceOf(it) else emptySequence() }
}
