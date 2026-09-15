package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermComparator
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
                        AssertAll.signature to AssertAll,
                        ArgumentHash.descriptionPair,
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

object ArgumentHash : BinaryRelation.WithoutSideEffects<ExecutionContext>("hash") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                Numeric.of(canonical(first).toString().hashCode()),
            ),
        )

    // Variables get renamed whenever a term is read back from the KB, so they are replaced by
    // positional placeholders: copies of the same term which only differ in variable names share the hash
    private fun canonical(term: Term): Term {
        val variables = term.variables.distinct().toList()
        if (variables.isEmpty()) return term
        return term.apply(Substitution.of(variables.mapIndexed { i, v -> v to Atom.of("_V$i") }.toMap()))
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
                list,
            )
        }

        return sequence {
            yield(
                request.replyWith(list.toSequence().any { elemList.toSequence().any { elem -> Unificator.default.match(it, elem) } }),
            )
        }
    }
}
