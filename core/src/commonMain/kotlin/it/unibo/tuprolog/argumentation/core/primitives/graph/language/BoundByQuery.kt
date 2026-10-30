package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * boundByQuery(+Rules, -Ids): the identifiers of Rules, `rule(Id, Premises, Conclusion)` terms as written, whose
 * arguments a query could bind otherwise than the whole graph derives them: a variable of the conclusion that no
 * ordinary premise has, or one that a Prolog check (`prolog(Goal)`) uses before the ordinary premises written before
 * it bind it (`X is Expr` binds X). For the other rules the search steered by a query finds the arguments the whole
 * graph has; contraries and constraints (`{ C }`) bind nothing.
 */
internal object BoundByQuery : BinaryRelation.WithoutSideEffects<ExecutionContext>("boundByQuery") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        val ids =
            first.asList()!!.toList().mapNotNull { rule ->
                val struct = rule.castToStruct()
                struct[0].takeIf { boundByQuery(struct[1].asList()!!.toList(), struct[2]) }
            }
        val unifier = Arg2pUnificator.mgu(second, List.of(ids))
        return if (unifier.isSuccess) sequenceOf(unifier) else emptySequence()
    }

    private fun boundByQuery(
        premises: kotlin.collections.List<Term>,
        conclusion: Term,
    ): Boolean {
        val bound = mutableSetOf<Var>()
        for (premise in premises) {
            val struct = premise as? Struct
            when {
                struct != null && struct.arity == 1 && struct.functor == "prolog" -> if (!checked(struct[0], bound)) return true
                struct != null && struct.arity == 1 && (struct.functor == "~" || struct.functor == "{}") -> Unit
                else -> bound += premise.variables
            }
        }
        return conclusion.variables.any { it !in bound }
    }

    // The check has its variables bound when it runs: a conjunction in its order, `X is Expr` binding X
    private fun checked(
        goal: Term,
        bound: MutableSet<Var>,
    ): Boolean {
        val struct = goal as? Struct
        return when {
            struct != null && struct.arity == 2 && struct.functor == "," -> checked(struct[0], bound) && checked(struct[1], bound)
            struct != null && struct.arity == 2 && struct.functor == "is" && struct[0] is Var ->
                struct[1].variables.all { it in bound }.also { bound += struct[0] as Var }
            else -> goal.variables.all { it in bound }
        }
    }
}
