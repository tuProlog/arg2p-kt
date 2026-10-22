package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.libs.data.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.libs.utils.distinctSortedTerms
import it.unibo.tuprolog.argumentation.core.libs.utils.sortedTerms
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * argument_rules(+RuleId, +Strict, +[Rules, LastDefRules, DefRules, DefPremises], ?SortedRules, ?Info): the rules and
 * the info of an argument built from RuleId on top of some sub-arguments, given the lists of their rules, last
 * defeasible rules, defeasible rules and defeasible premises; Strict tells whether RuleId is strict (strict) or not.
 * Fails if RuleId already occurs among the rules of the sub-arguments, or if it is none and not strict.
 * SortedRules: RuleId and the rules, as sort/2. Info: [LastDefRules, DefRules, DefPremises], each as sortDistinct/2.
 */
object ArgumentRules : Primitive {
    val signature: Signature = Signature("argument_rules", 5)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val ruleId = request.arguments[0]
        val strict = request.arguments[1].let { it is Atom && it.value == "strict" }
        val parts = request.arguments[2].let { it as? List }?.toList()
        if (parts == null || parts.size != 4) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.LIST, request.arguments[2])
        }
        val (all, lastDef, def, premises) =
            parts.map { lists ->
                (lists as? List ?: throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.LIST, lists))
                    .toList()
                    .flatMap {
                        (
                            it as? List ?: throw TypeError.forGoal(
                                request.context,
                                request.signature,
                                TypeError.Expected.LIST,
                                it,
                            )
                        ).toList()
                    }
            }
        if (all.any { Arg2pUnificator.match(it, ruleId) }) return emptySequence()
        val (newLastDef, newDef) =
            when {
                strict -> lastDef to def
                ruleId is Atom && ruleId.value == "none" -> return emptySequence()
                else -> listOf(ruleId) to listOf(ruleId) + def
            }
        val info =
            ArgumentTerm.info(
                List.of(List.of(newLastDef).distinctSortedTerms().asReversed()),
                List.of(List.of(newDef).distinctSortedTerms().asReversed()),
                List.of(List.of(premises).distinctSortedTerms().asReversed()),
            )
        val rules = List.of(List.of(listOf(ruleId) + all).sortedTerms().asReversed())
        val unifier = Arg2pUnificator.mgu(Tuple.of(request.arguments[3], request.arguments[4]), Tuple.of(rules, info))
        return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
    }
}
