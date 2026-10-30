package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * ruleIds(+Rules, ?Ids): Ids are the identifiers of Rules, the lists `[Id, ...]` the parser reads, in their order: a
 * single visit of them, which a Prolog loop over the list would make quadratic (each step applying its bindings to the
 * rest of the list)
 */
internal object RuleIds : BinaryRelation.WithoutSideEffects<ExecutionContext>("ruleIds") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        val ids = first.asList()!!.toList().mapNotNull { it.asList()?.toList()?.firstOrNull() }
        val unifier = mgu(second, List.of(ids))
        return if (unifier.isSuccess) sequenceOf(unifier) else emptySequence()
    }
}
