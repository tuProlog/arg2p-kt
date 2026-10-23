package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * intersection(+Sets, -Intersection): the elements of the first list in Sets that are in all of them, in its order
 * (elements compared as ground terms); fails if Sets is empty
 */
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
