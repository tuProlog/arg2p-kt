package it.unibo.tuprolog.argumentation.core.primitives.data

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.primitives.utils.distinctSortedTerms
import it.unibo.tuprolog.argumentation.core.primitives.utils.sortedTerms
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/**
 * argument::rulesAndInfo(+RuleId, +Strict, +[Rules, LastDefRules, DefRules, DefPremises], ?SortedRules, ?Info): the rules and
 * the info of an argument built from RuleId on top of some sub-arguments, given the lists of their rules, last
 * defeasible rules, defeasible rules and defeasible premises; Strict tells whether RuleId is strict (strict) or not.
 * Fails if RuleId already occurs among the rules of the sub-arguments, or if it is none and not strict.
 * SortedRules: RuleId and the rules, as sort/2. Info: [LastDefRules, DefRules, DefPremises], each as sortDistinct/2.
 */
internal object RulesAndInfo : AbstractNativePrimitive(Signature("rulesAndInfo", 5)) {
    override fun call(call: NativeCall): Int {
        val ruleId = call.termArg(0)
        val strict = call.termArg(1).let { it is Atom && it.value == "strict" }
        val partsTerm = call.termArg(2)
        val parts = (partsTerm as? List)?.toList()
        if (parts == null || parts.size != 4) {
            throw TypeError.forGoal(call.context, signature, TypeError.Expected.LIST, partsTerm)
        }
        val (all, lastDef, def, premises) =
            parts.map { lists ->
                (lists as? List ?: throw TypeError.forGoal(call.context, signature, TypeError.Expected.LIST, lists))
                    .toList()
                    .flatMap { (it as? List ?: throw TypeError.forGoal(call.context, signature, TypeError.Expected.LIST, it)).toList() }
            }
        if (all.any { Arg2pUnificator.match(it, ruleId) }) return NativeCall.FAIL
        val (newLastDef, newDef) =
            when {
                strict -> lastDef to def
                ruleId is Atom && ruleId.value == "none" -> return NativeCall.FAIL
                else -> listOf(ruleId) to listOf(ruleId) + def
            }
        val info =
            ArgumentTerm.info(
                List.of(List.of(newLastDef).distinctSortedTerms().asReversed()),
                List.of(List.of(newDef).distinctSortedTerms().asReversed()),
                List.of(List.of(premises).distinctSortedTerms().asReversed()),
            )
        val rules = List.of(List.of(listOf(ruleId) + all).sortedTerms().asReversed())
        if (call.reply(3, rules) == NativeCall.FAIL) return NativeCall.FAIL
        return call.reply(4, info)
    }
}
