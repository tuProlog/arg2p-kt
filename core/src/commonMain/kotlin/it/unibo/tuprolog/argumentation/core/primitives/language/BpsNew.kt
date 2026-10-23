package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * bpsNew(-Bps): the burdens of persuasion of the theory, its `bp(...)` facts, as `abstractBp([...])` terms
 */
object BpsNew : UnaryPredicate.WithoutSideEffects<ExecutionContext>("bpsNew") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first, true) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && it.value.head?.functor == "bp" }
                .map { Struct.of("abstractBp", prologScope.logicListOf(it.value.head!!.args)) }
                .toTerm()
        }
}
