package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * prologPremises(-Premises): the facts of the theory written `:= Fact` as defeasible premises, `[Id, Fact]`, with
 * prologStrictCompatibility on (none otherwise)
 */
object PrologPremises : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologPremises") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && it.value.head!!.functor == ":=" && it.value.head!!.arity == 1 }
                .map { (index, clause) -> prologScope.logicListOf(ConversionUtils.ruleId(index), clause.head!!.args[0]) }
                .toTerm()
        }
}
