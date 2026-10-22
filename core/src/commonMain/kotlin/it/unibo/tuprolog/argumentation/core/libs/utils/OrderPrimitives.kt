package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermComparator
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

// Sorting utilities, natively implemented as they are invoked for every argument (module utils)

internal fun unifyWith(
    target: Term,
    result: Term,
): Sequence<Substitution> = Arg2pUnificator.mgu(target, result).let { if (it.isSuccess) sequenceOf(it) else emptySequence() }

internal fun List.sortedTerms(): kotlin.collections.List<Term> = toList().sortedWith(TermComparator.DefaultComparator)

// Standard order of terms, duplicates removed, and variables renamed apart as in setof/3
internal fun List.distinctSortedTerms(): kotlin.collections.List<Term> =
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
