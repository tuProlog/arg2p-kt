package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologPremises(-Premises): the Prolog facts of the theory with an identifier (see [RuleAnnotations]) as
 * defeasible premises, `[Id, Fact]`, with prologStrictCompatibility on (none otherwise)
 */
internal object PrologPremises : AbstractNativePrimitive(Signature("prologPremises", 1)) {
    override fun call(call: NativeCall): Int =
        ConversionUtils.commonMap(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> ConversionUtils.isPrologFact(clause) }
                .mapNotNull { (_, clause) ->
                    RuleAnnotations.of(clause)?.let { prologScope.logicListOf(Atom.of(it), clause.head!!) }
                }.toTerm()
        }
}
