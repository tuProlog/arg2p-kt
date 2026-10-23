package it.unibo.tuprolog.argumentation.core.primitives.graph.partial

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * groundVars(+Term, ?Grounded): Term with each of its variables replaced by the atom var
 */
object GroundVars : BinaryRelation.WithoutSideEffects<ExecutionContext>("groundVars") {
    private val placeholder = Atom.of("var")

    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        Arg2pUnificator
            .mgu(second, first.apply(Substitution.of(first.variables.distinct().associateWith { placeholder })))
            .let { if (it.isSuccess) sequenceOf(it) else emptySequence() }
}
