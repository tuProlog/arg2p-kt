package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.unify.Unificator

/**
 * needsIndex(+Rules, -Index): for Rules, `rule(Id, Conclusion, Premises)` terms, `checkNeeds(Conclusion, Vars)` and
 * `needingRule(Id)` for each rule needing the variables Vars of its conclusion bound by the statement asked for (see parser::complete/1). A rule
 * needs a variable of its conclusion that a Prolog check (`prolog(Goal)`) uses before an ordinary premise written
 * before it binds it, or that a premise needs and no other premise binds: a premise needs the variables the rules
 * concluding it need, and binds the others. A rule also needs the variables within the regions of its conclusion (see
 * the module constraints). The needs of the premises grow with those of the rules concluding them, until none does.
 */
internal object NeedsIndex : BinaryRelation.WithoutSideEffects<ExecutionContext>("needsIndex") {
    private class Rule(
        val id: Term,
        val conclusion: Struct,
        val premises: kotlin.collections.List<Term>,
    )

    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        val rules =
            first.asList()!!.toList().map {
                val rule = it.castToStruct()
                Rule(rule[0], rule[1].castToStruct(), rule[2].asList()!!.toList())
            }
        val byConclusion = rules.indices.groupBy { rules[it].conclusion.indicator }
        val needs = Array(rules.size) { emptySet<Var>() }
        do {
            var grown = false
            rules.forEachIndexed { i, rule ->
                val now = ruleNeeds(rule, rules, byConclusion, needs)
                if (now.size > needs[i].size) {
                    needs[i] = now
                    grown = true
                }
            }
        } while (grown)
        val index =
            rules.indices
                .filter { needs[it].isNotEmpty() }
                .flatMap {
                    listOf(
                        Struct.of("checkNeeds", rules[it].conclusion, List.of(needs[it].toList())),
                        Struct.of("needingRule", rules[it].id),
                    )
                }
        val unifier = Arg2pUnificator.mgu(second, List.of(index))
        return if (unifier.isSuccess) sequenceOf(unifier) else emptySequence()
    }

    // The variables of the conclusion of rule it needs, with the needs of the rules found so far
    private fun ruleNeeds(
        rule: Rule,
        rules: kotlin.collections.List<Rule>,
        byConclusion: Map<Indicator, kotlin.collections.List<Int>>,
        needs: Array<Set<Var>>,
    ): Set<Var> {
        val bound = mutableSetOf<Var>()
        val checkNeeded = mutableSetOf<Var>()
        val premiseNeeded = mutableSetOf<Var>()
        for (premise in rule.premises) {
            val struct = premise as? Struct
            when {
                struct != null && struct.arity == 1 && struct.functor == "prolog" -> check(struct[0], bound, checkNeeded)
                struct != null && struct.arity == 1 && struct.functor == "~" -> Unit
                else -> {
                    val premiseNeeds = premiseNeeds(premise, rules, byConclusion, needs)
                    premise.variables.filterNot { it in premiseNeeds }.forEach { bound += it }
                    premiseNeeded += premiseNeeds
                }
            }
        }
        val inRegions = regionVariables(rule.conclusion)
        return rule.conclusion.variables
            .filter { it in checkNeeded || (it in premiseNeeded && it !in bound) || it in inRegions }
            .toSet()
    }

    // The variables a check needs bound, those it uses before they are: a conjunction in its order, and `X is Expr`
    // binding X (the result) and needing those of Expr alone; any other goal needs all its variables
    private fun check(
        goal: Term,
        bound: MutableSet<Var>,
        needed: MutableSet<Var>,
    ) {
        val struct = goal as? Struct
        when {
            struct != null && struct.arity == 2 && struct.functor == "," -> {
                check(struct[0], bound, needed)
                check(struct[1], bound, needed)
            }
            struct != null && struct.arity == 2 && struct.functor == "is" && struct[0] is Var -> {
                struct[1].variables.filterNot { it in bound }.forEach { needed += it }
                bound += struct[0] as Var
            }
            else -> goal.variables.filterNot { it in bound }.forEach { needed += it }
        }
    }

    // The variables within the region terms of a conclusion (from(T), until(T), between(A, B), see the module
    // constraints): a statement asked for with a variable there gets the region, not a number, which a check or an
    // arithmetic on it could not take, so the rule needs it bound
    private fun regionVariables(term: Term): Set<Var> =
        when {
            term !is Struct -> emptySet()
            (term.arity == 1 && (term.functor == "from" || term.functor == "until")) || (term.arity == 2 && term.functor == "between") ->
                term.variables.toSet()
            else -> term.args.flatMap { regionVariables(it) }.toSet()
        }

    // The variables of premise that the rules concluding it need: those within the terms a needed variable meets
    private fun premiseNeeds(
        premise: Term,
        rules: kotlin.collections.List<Rule>,
        byConclusion: Map<Indicator, kotlin.collections.List<Int>>,
        needs: Array<Set<Var>>,
    ): Set<Var> {
        val struct = premise as? Struct ?: return emptySet()
        val candidates = byConclusion[struct.indicator] ?: return emptySet()
        val premiseVars = premise.variables.toSet()
        val found = mutableSetOf<Var>()
        for (j in candidates) {
            if (needs[j].isEmpty()) continue
            // the rule renamed apart from the premise (it may be the rule of the premise itself)
            val fresh = Tuple.of(rules[j].conclusion, List.of(needs[j].toList())).freshCopy() as Tuple
            val needed =
                fresh.right
                    .asList()!!
                    .toList()
                    .map { it.castToVar() }
                    .toSet()
            val unifier = Unificator.default.mgu(fresh.left, premise, true)
            if (!unifier.isSuccess) continue
            needed.forEach { v ->
                v
                    .apply(unifier)
                    .variables
                    .filter { it in premiseVars }
                    .forEach { found += it }
            }
            premiseVars.forEach { v -> if (v.apply(unifier).variables.any { it in needed }) found += v }
        }
        return found
    }
}
