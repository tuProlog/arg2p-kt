package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * minimalBySet(+Pairs, -Values): the values of the Set-Value pairs whose Set has no strict subset among the others
 * (elements compared as ground terms), in order: minimal_set/2 over a whole collection in one step
 */
internal object MinimalBySet : BinaryRelation.WithoutSideEffects<ExecutionContext>("minimalBySet") {
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
