package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermComparator
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.unify.Unificator

abstract class UtilsBase :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.utils"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
                primitives =
                    mapOf(
                        AppendOptimized.signature to AppendOptimized,
                        AppendLists.signature to AppendLists,
                        ArgumentRules.signature to ArgumentRules,
                        AssertAll.signature to AssertAll,
                        Contains.signature to Contains,
                        ContainsAny.signature to ContainsAny,
                        SortTerms.descriptionPair,
                        Deduplicate.descriptionPair,
                        SortDistinct.descriptionPair,
                    ),
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "utils"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)
}

expect object Utils : UtilsBase {
    override val prologRawTheory: String
}

object AppendOptimized : Primitive {
    val signature: Signature = Signature("append_fast", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val list1: Term = request.arguments[0]
        val list2: Term = request.arguments[1]
        val res: Term = request.arguments[2]

        if (list1 !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list1,
            )
        }

        if (list2 !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list2,
            )
        }

        if (res !is Var) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.VARIABLE,
                res,
            )
        }

        return sequence {
            yield(
                request.replyWith(Substitution.of(res, List.of(list1.toList() + list2.toList()))),
            )
        }
    }
}

/** appendLists(Lists, Result): Result is the concatenation of the lists in Lists */
object AppendLists : Primitive {
    val signature: Signature = Signature("appendLists", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val lists: Term = request.arguments[0]

        if (lists !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                lists,
            )
        }

        val items =
            lists.toList().flatMap { list ->
                if (list !is List) {
                    throw TypeError.forGoal(
                        request.context,
                        request.signature,
                        TypeError.Expected.LIST,
                        list,
                    )
                }
                list.toList()
            }
        val unifier = Unificator.default.mgu(request.arguments[1], List.of(items))
        return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
    }
}

object AssertAll : Primitive {
    val signature: Signature = Signature("assert_all", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val list: Term = request.arguments[0]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list,
            )
        }

        arg2pScope {
            list.toList().forEach { request.solve("context_assert"(it)).first() }
        }

        return sequence {
            yield(
                request.replyWith(true),
            )
        }
    }
}

// Sorting utilities, natively implemented as they are invoked for every argument

private fun unifyWith(
    target: Term,
    result: Term,
): Sequence<Substitution> = Unificator.default.mgu(target, result).let { if (it.isSuccess) sequenceOf(it) else emptySequence() }

private fun List.sortedTerms(): kotlin.collections.List<Term> = toList().sortedWith(TermComparator.DefaultComparator)

// Standard order of terms, duplicates removed, and variables renamed apart as in setof/3
private fun List.distinctSortedTerms(): kotlin.collections.List<Term> =
    sortedTerms()
        .fold(mutableListOf<Term>()) { acc, term ->
            acc.also { if (it.isEmpty() || TermComparator.DefaultComparator.compare(it.last(), term) != 0) it.add(term) }
        }.map { it.freshCopy() }

// sort(+List, ?Sorted): reverse standard order of terms, duplicates kept (as the former quicksort)
object SortTerms : BinaryRelation.WithoutSideEffects<ExecutionContext>("sort") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (first is List) unifyWith(second, List.of(first.sortedTerms().asReversed())) else emptySequence()
}

// deduplicate(+List, ?Deduplicated): standard order of terms, duplicates removed (as setof/3)
object Deduplicate : BinaryRelation.WithoutSideEffects<ExecutionContext>("deduplicate") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (first is List) unifyWith(second, List.of(first.distinctSortedTerms())) else emptySequence()
}

// sortDistinct(+List, ?Sorted): reverse standard order of terms, duplicates removed (deduplicate/2, then sort/2)
object SortDistinct : BinaryRelation.WithoutSideEffects<ExecutionContext>("sortDistinct") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (first is List) unifyWith(second, List.of(first.distinctSortedTerms().asReversed())) else emptySequence()
}

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
        if (all.any { Unificator.default.match(it, ruleId) }) return emptySequence()
        val (newLastDef, newDef) =
            when {
                strict -> lastDef to def
                ruleId is Atom && ruleId.value == "none" -> return emptySequence()
                else -> listOf(ruleId) to listOf(ruleId) + def
            }
        val info =
            List.of(
                List
                    .of(newLastDef)
                    .distinctSortedTerms()
                    .asReversed()
                    .let { List.of(it) },
                List
                    .of(newDef)
                    .distinctSortedTerms()
                    .asReversed()
                    .let { List.of(it) },
                List
                    .of(premises)
                    .distinctSortedTerms()
                    .asReversed()
                    .let { List.of(it) },
            )
        val rules = List.of(List.of(listOf(ruleId) + all).sortedTerms().asReversed())
        val unifier = Unificator.default.mgu(Tuple.of(request.arguments[3], request.arguments[4]), Tuple.of(rules, info))
        return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
    }
}

object Contains : Primitive {
    val signature: Signature = Signature("contains", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val elem: Term = request.arguments[0]
        val list: Term = request.arguments[1]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list,
            )
        }

        return sequence {
            yield(
                request.replyWith(list.toList().any { Unificator.default.match(it, elem) }),
            )
        }
    }
}

object ContainsAny : Primitive {
    val signature: Signature = Signature("contains_any", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val elemList: Term = request.arguments[0]
        val list: Term = request.arguments[1]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list,
            )
        }

        if (elemList !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                elemList,
            )
        }

        return sequence {
            yield(
                request.replyWith(list.toSequence().any { elemList.toSequence().any { elem -> Unificator.default.match(it, elem) } }),
            )
        }
    }
}
