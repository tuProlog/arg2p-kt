package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * sortDescending(+List, ?Sorted): the reverse of the standard order of terms, duplicates kept (as the former quicksort)
 */
internal object Sort : BinaryRelation.WithoutSideEffects<ExecutionContext>("sortDescending") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (first is List) unifyWith(second, List.of(first.sortedTerms().asReversed())) else emptySequence()
}
