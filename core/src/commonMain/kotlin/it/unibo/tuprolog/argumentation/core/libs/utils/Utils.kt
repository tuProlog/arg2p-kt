package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.argumentation.core.libs.graph.argumentIds
import it.unibo.tuprolog.argumentation.core.libs.graph.attacks
import it.unibo.tuprolog.argumentation.core.libs.graph.ids
import it.unibo.tuprolog.argumentation.core.libs.graph.replyList
import it.unibo.tuprolog.argumentation.core.libs.graph.stronglyConnectedComponents
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
import it.unibo.tuprolog.solve.primitive.TernaryRelation

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
                        ForEach.signature to ForEach,
                        Contains.signature to Contains,
                        ContainsAny.signature to ContainsAny,
                        SortTerms.descriptionPair,
                        Deduplicate.descriptionPair,
                        SortDistinct.descriptionPair,
                        MinimalBySet.descriptionPair,
                        Element.descriptionPair,
                        Reversed.descriptionPair,
                        UniqueByFirst.descriptionPair,
                        Intersection.descriptionPair,
                        Subtract.descriptionPair,
                        StronglyConnectedComponents(1).let { it.signature to it },
                        StronglyConnectedComponents(2).let { it.signature to it },
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
        val unifier = Arg2pUnificator.mgu(request.arguments[1], List.of(items))
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

/**
 * for_each(?Element, +List, +Goal): Goal (all its solutions) for each element of List, as
 * findall(_, (member(Element, List), Goal), _), but without List in the goals of the iterations: with variable
 * tracking, every resolution step visits the terms of the goal it belongs to, so the list would make every step
 * as costly as it is long. Goal runs in this module: other modules are called through ::.
 */
object ForEach : Primitive {
    val signature: Signature = Signature("for_each", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val list = request.arguments[1]
        if (list !is List) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.LIST, list)
        }
        list.toList().forEach { item ->
            val iteration = Tuple.of(request.arguments[0], request.arguments[2]).freshCopy()
            val unifier = Arg2pUnificator.mgu(iteration[0], item)
            if (unifier.isSuccess) {
                request.solve(iteration[1].apply(unifier).castToStruct()).forEach { if (it.isHalt) throw it.exception!! }
            }
        }
        return sequenceOf(request.replyWith(true))
    }
}

private fun unifyWith(
    target: Term,
    result: Term,
): Sequence<Substitution> = Arg2pUnificator.mgu(target, result).let { if (it.isSuccess) sequenceOf(it) else emptySequence() }

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

// intersection(+Sets, -Intersection): the elements of the first list in Sets that are in all of them, in its order
// (elements compared as ground terms); fails if Sets is empty
object Intersection : BinaryRelation.WithoutSideEffects<ExecutionContext>("intersection") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        if (first !is List) return emptySequence()
        val sets = first.toList().map { (it as? List)?.toList() ?: return emptySequence() }
        if (sets.isEmpty()) return emptySequence()
        val others = sets.drop(1).map { it.toSet() }
        return unifyWith(second, List.of(sets.first().filter { y -> others.all { y in it } }))
    }
}

// subtract(+List, +Removed, ?Rest): the elements of List not in Removed, in order (elements compared as ground terms),
// without the recursion of a Prolog subtract (whose frames would all keep the rest of the list)
object Subtract : TernaryRelation.WithoutSideEffects<ExecutionContext>("subtract") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
        third: Term,
    ): Sequence<Substitution> {
        if (first !is List || second !is List) return emptySequence()
        val removed = second.toList().toSet()
        return unifyWith(third, List.of(first.toList().filter { it !in removed }))
    }
}

/**
 * sccs(-Sccs): strongly connected components of the attack graph, as lists of argument identifiers,
 * in topological order (a component is only attacked by itself and by the components before it).
 * sccs(+Ids, -Sccs): the same, for the subgraph induced by the arguments in Ids.
 */
class StronglyConnectedComponents(
    arity: Int,
) : Primitive {
    val signature: Signature = Signature("sccs", arity)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val nodes = if (request.arguments.size == 1) request.argumentIds() else request.ids(0)
        val attacked = request.attacks().groupBy({ it.first }, { it.second })
        val sccs = stronglyConnectedComponents(nodes) { attacked[it].orEmpty() }
        return sequenceOf(request.replyList(request.arguments.lastIndex, sccs.map { List.of(it) }) ?: request.replyFail())
    }
}

// element(?Element, +List): Element unified with each element of List in turn, as member/2 does, but from a single
// choice point: member/2 recurses, keeping an execution context for each element enumerated so far
object Element : BinaryRelation.WithoutSideEffects<ExecutionContext>("element") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (second is List) second.toSequence().flatMap { unifyWith(first, it) } else emptySequence()
}

// reversed(+List, ?Reversed): the elements of List in reverse order, without the recursion of a Prolog reverse
// (whose frames would all keep the rest of the list)
object Reversed : BinaryRelation.WithoutSideEffects<ExecutionContext>("reversed") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (first is List) unifyWith(second, List.of(first.toList().asReversed())) else emptySequence()
}

// unique_by_first(+Lists, ?Unique): Lists keeping, among the lists with the same first element, only the last one
// (in their order)
object UniqueByFirst : BinaryRelation.WithoutSideEffects<ExecutionContext>("unique_by_first") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        if (first !is List) return emptySequence()
        val seen = HashSet<Term>()
        val unique =
            first
                .toList()
                .asReversed()
                .filter { seen.add(it.castToList().toList().first()) }
                .asReversed()
        return unifyWith(second, List.of(unique))
    }
}

// minimal_by_set(+Pairs, -Values): the values of the Set-Value pairs whose Set has no strict subset among the others
// (elements compared as ground terms), in order: minimal_set/2 over a whole collection in one step
object MinimalBySet : BinaryRelation.WithoutSideEffects<ExecutionContext>("minimal_by_set") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        if (first !is List) return emptySequence()
        val pairs =
            first.toList().map { pair ->
                val struct = pair.castToStruct()
                ((struct[0] as? List)?.toList()?.toSet() ?: return emptySequence()) to struct[1]
            }
        val sets = pairs.map { it.first }.distinct()
        val minimal = pairs.filter { (set, _) -> sets.none { other -> other != set && set.containsAll(other) } }
        return unifyWith(second, List.of(minimal.map { it.second }))
    }
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
        if (all.any { Arg2pUnificator.match(it, ruleId) }) return emptySequence()
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
        val unifier = Arg2pUnificator.mgu(Tuple.of(request.arguments[3], request.arguments[4]), Tuple.of(rules, info))
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
                request.replyWith(list.toList().any { Arg2pUnificator.match(it, elem) }),
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
                request.replyWith(list.toSequence().any { elemList.toSequence().any { elem -> Arg2pUnificator.match(it, elem) } }),
            )
        }
    }
}
