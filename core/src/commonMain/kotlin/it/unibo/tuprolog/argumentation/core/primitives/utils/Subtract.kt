package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.TernaryRelation

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
