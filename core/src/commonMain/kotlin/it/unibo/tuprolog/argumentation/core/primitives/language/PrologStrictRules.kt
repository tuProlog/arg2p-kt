package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * prologStrictRules(-Rules): the Prolog clauses of the theory (Head :- Body) as strict rules, `[Id, Body, Head]`,
 * with prologStrictCompatibility on (none otherwise)
 */
object PrologStrictRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { !it.value.isFact }
                .map { (index, clause) ->
                    prologScope.logicListOf(
                        ConversionUtils.ruleId(index),
                        ConversionUtils.modifiers(clause, this@computeAllSubstitutions),
                        clause.head!!,
                    )
                }.toTerm()
        }
}
