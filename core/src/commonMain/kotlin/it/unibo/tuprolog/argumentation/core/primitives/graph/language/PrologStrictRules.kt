package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * prologStrictRules(-Rules): the Prolog clauses of the theory (Head :- Body) with no identifier (see
 * [RuleAnnotations]) as strict rules, `[Id, Body, Head]`, with prologStrictCompatibility on (none otherwise)
 */
internal object PrologStrictRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> clause.isRule && RuleAnnotations.of(clause) == null }
                .map { (index, clause) ->
                    prologScope.logicListOf(
                        ConversionUtils.ruleId(index),
                        ConversionUtils.modifiers(clause, this@computeAllSubstitutions),
                        clause.head!!,
                    )
                }.toTerm()
        }
}
