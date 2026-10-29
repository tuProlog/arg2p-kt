package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * uniqueByFirst(+Lists, ?Unique): Lists keeping, among the lists with the same first element, only the last one
 * (in their order)
 */
internal object UniqueByFirst : BinaryRelation.WithoutSideEffects<ExecutionContext>("uniqueByFirst") {
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
