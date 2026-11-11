package it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.TheoryClauses
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.helpers.ClauseConversion
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologStrictRules(-Rules): the Prolog clauses of the theory (Head :- Body) with no identifier (see
 * [RuleAnnotations]) as strict rules, `[Id, Body, Head]`
 */
internal object PrologStrictRules : AbstractNativePrimitive(Signature("prologStrictRules", 1)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        return TheoryClauses.reply(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> clause.isRule && RuleAnnotations.of(clause) == null }
                .map { (index, clause) ->
                    prologScope.logicListOf(ClauseConversion.ruleId(index), ClauseConversion.modifiers(clause, context), clause.head!!)
                }.toTerm()
        }
    }
}
