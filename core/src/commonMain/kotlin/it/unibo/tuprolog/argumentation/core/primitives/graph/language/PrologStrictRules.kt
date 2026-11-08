package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologStrictRules(-Rules): the Prolog clauses of the theory (Head :- Body) with no identifier (see
 * [RuleAnnotations]) as strict rules, `[Id, Body, Head]`, with prologStrictCompatibility on (none otherwise)
 */
internal object PrologStrictRules : AbstractNativePrimitive(Signature("prologStrictRules", 1)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        return ConversionUtils.commonMap(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> clause.isRule && RuleAnnotations.of(clause) == null }
                .map { (index, clause) ->
                    prologScope.logicListOf(ConversionUtils.ruleId(index), ConversionUtils.modifiers(clause, context), clause.head!!)
                }.toTerm()
        }
    }
}
