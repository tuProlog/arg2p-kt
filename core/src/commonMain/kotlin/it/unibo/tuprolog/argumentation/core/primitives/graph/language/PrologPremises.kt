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
 * prologPremises(-Premises): the Prolog facts of the theory with an identifier (see [RuleAnnotations]) as
 * defeasible premises, `[Id, Fact]`, with prologStrictCompatibility on (none otherwise)
 */
object PrologPremises : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologPremises") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> ConversionUtils.isPrologFact(clause) }
                .mapNotNull { (_, clause) ->
                    RuleAnnotations.of(clause)?.let { prologScope.logicListOf(Atom.of(it), clause.head!!) }
                }.toTerm()
        }
}
