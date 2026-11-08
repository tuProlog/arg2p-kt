package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologDefeasibleRules(-Rules): the Prolog clauses of the theory (Head :- Body) with an identifier (see
 * [RuleAnnotations]) as defeasible rules, `[Id, Body, Head]`, with prologStrictCompatibility on (none otherwise)
 */
internal object PrologDefeasibleRules : AbstractNativePrimitive(Signature("prologDefeasibleRules", 1)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        return ConversionUtils.commonMap(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> clause.isRule }
                .mapNotNull { (_, clause) ->
                    RuleAnnotations.of(clause)?.let {
                        prologScope.logicListOf(Atom.of(it), ConversionUtils.modifiers(clause, context), clause.head!!)
                    }
                }.toTerm()
        }
    }
}
