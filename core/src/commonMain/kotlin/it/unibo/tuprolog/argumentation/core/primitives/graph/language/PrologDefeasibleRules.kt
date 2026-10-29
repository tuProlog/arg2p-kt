package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * prologDefeasibleRules(-Rules): the Prolog clauses of the theory (Head :- Body) with an identifier (see
 * [RuleAnnotations]) as defeasible rules, `[Id, Body, Head]`, with prologStrictCompatibility on (none otherwise)
 */
internal object PrologDefeasibleRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologDefeasibleRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> clause.isRule }
                .mapNotNull { (_, clause) ->
                    RuleAnnotations.of(clause)?.let {
                        prologScope.logicListOf(
                            Atom.of(it),
                            ConversionUtils.modifiers(clause, this@computeAllSubstitutions),
                            clause.head!!,
                        )
                    }
                }.toTerm()
        }
}
