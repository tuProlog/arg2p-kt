package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * sort(+List, ?Sorted): reverse standard order of terms, duplicates kept (as the former quicksort)
 */
object Sort : BinaryRelation.WithoutSideEffects<ExecutionContext>("sort") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> = if (first is List) unifyWith(second, List.of(first.sortedTerms().asReversed())) else emptySequence()
}
