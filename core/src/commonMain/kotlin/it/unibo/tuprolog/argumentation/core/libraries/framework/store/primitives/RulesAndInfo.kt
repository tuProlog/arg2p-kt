package it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.distinctSortedTerms
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.sortedTerms
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.items
import it.unibo.tuprolog.argumentation.engine.natives.reply
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/**
 * store::rulesAndInfo(+RuleId, +Strict, +[Rules, LastDefRules, DefRules, DefPremises], ?SortedRules, ?Info): the rules and
 * the info of an argument built from RuleId on top of some sub-arguments, given the lists of their rules, last
 * defeasible rules, defeasible rules and defeasible premises; Strict tells whether RuleId is strict (strict) or not.
 * Fails if RuleId already occurs among the rules of the sub-arguments, or if it is none and not strict.
 * SortedRules: RuleId and the rules, as sort/2. Info: [LastDefRules, DefRules, DefPremises], each as sortDistinct/2.
 */
internal object RulesAndInfo : AbstractNativePrimitive(Signature("rulesAndInfo", 5)) {
    override fun call(call: NativeCall): Int = onWords(call) ?: onTerms(call)

    // The rules and the info on the words of the machine; null when the terms are to be read (a variable in what is
    // sorted, a part that is not a list of lists: an error)
    private fun onWords(call: NativeCall): Int? {
        val parts = call.items(call.arg(2))?.takeIf { it.size == 4 } ?: return null
        val flat =
            parts.map { part ->
                val lists = call.items(part) ?: return null
                var all = IntArray(0)
                for (list in lists) all += call.items(list) ?: return null
                all
            }
        val ruleId = call.arg(0)
        val strictWord = call.arg(1)
        val strict = call.kind(strictWord) == NativeCall.ATOM && call.atomName(strictWord) == "strict"
        if (flat[0].any { call.unifiable(it, ruleId) }) return NativeCall.FAIL
        val lastDef: IntArray
        val def: IntArray
        if (strict) {
            lastDef = flat[1]
            def = flat[2]
        } else {
            if (call.kind(ruleId) == NativeCall.ATOM && call.atomName(ruleId) == "none") return NativeCall.FAIL
            lastDef = intArrayOf(ruleId)
            def = intArrayOf(ruleId) + flat[2]
        }
        val sortedLast = call.sortedDistinct(lastDef) ?: return null
        val sortedDef = call.sortedDistinct(def) ?: return null
        val sortedPremises = call.sortedDistinct(flat[3]) ?: return null
        val sortedRules = call.sorted(intArrayOf(ruleId) + flat[0]) ?: return null
        val info =
            call.list(
                intArrayOf(
                    call.list(sortedLast.reversedArray()),
                    call.list(sortedDef.reversedArray()),
                    call.list(sortedPremises.reversedArray()),
                ),
            )
        return if (call.unify(call.arg(3), call.list(sortedRules.reversedArray()))) call.replyWord(4, info) else NativeCall.FAIL
    }

    private fun onTerms(call: NativeCall): Int {
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
