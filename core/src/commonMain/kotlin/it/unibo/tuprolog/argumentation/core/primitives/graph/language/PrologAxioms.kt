package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologAxioms(-Axioms): the Prolog facts of the theory with no identifier (see [RuleAnnotations]) as axioms,
 * `[Id, Fact]`, with prologStrictCompatibility on (none otherwise)
 */
internal object PrologAxioms : AbstractNativePrimitive(Signature("prologAxioms", 1)) {
    override fun call(call: NativeCall): Int =
        ConversionUtils.commonMap(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> ConversionUtils.isPrologFact(clause) && RuleAnnotations.of(clause) == null }
                .map { (index, clause) -> prologScope.logicListOf(ConversionUtils.ruleId(index), clause.head!!) }
                .toTerm()
        }
}
