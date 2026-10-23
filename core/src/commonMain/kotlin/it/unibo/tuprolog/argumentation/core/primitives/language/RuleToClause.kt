package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.argumentation.core.model.terms.RuleTerm
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * rule_to_clause(+Rules, -Clauses): the rules and premises of the parser (`[Id, Body, Head]`, `[Id, Head]`) as
 * the clauses of the module rule
 */
object RuleToClause : BinaryRelation.WithoutSideEffects<ExecutionContext>("rule_to_clause") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first
                        .asList()!!
                        .toList()
                        .map { original ->
                            RuleTerm.of(original.castToList()) ?: throw IllegalArgumentException()
                        },
                ),
            ),
        )
}
