package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * prologAxioms(-Axioms): the Prolog facts of the theory with no identifier (see [RuleAnnotations]) as axioms,
 * `[Id, Fact]`, with prologStrictCompatibility on (none otherwise)
 */
internal object PrologAxioms : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologAxioms") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> ConversionUtils.isPrologFact(clause) && RuleAnnotations.of(clause) == null }
                .map { (index, clause) -> prologScope.logicListOf(ConversionUtils.ruleId(index), clause.head!!) }
                .toTerm()
        }
}
