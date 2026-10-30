package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.TernaryRelation

/**
 * boundByQuery(+Rules, -Ids, -Constraints): of Rules, as the parser reads them from the theory (`[Id, Premises,
 * Conclusion]`, the premises a conjunction, a list or `[]`, and `[Id, Conclusion]` for a premise of the theory), Ids
 * are those whose arguments a query could bind otherwise than the whole graph derives them: a variable of the
 * conclusion that no ordinary premise has, or one that a Prolog check (`prolog(Goal)`) uses before the ordinary
 * premises written before it bind it (`X is Expr` binds X). For the others the search steered by a query finds the
 * arguments the whole graph has; contraries and constraints (`{ C }`) bind nothing. Constraints is `yes` if a premise
 * is a constraint, `no` otherwise. A single visit of the rules, which a Prolog loop over the list would make
 * quadratic (each step applying its bindings to the rest of the list).
 */
internal object BoundByQuery : TernaryRelation.WithoutSideEffects<ExecutionContext>("boundByQuery") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
        third: Term,
    ): Sequence<Substitution> {
        val ids = mutableListOf<Term>()
        var constraints = false
        for (rule in first.asList()!!.toList()) {
            val parts = rule.asList()?.toList() ?: continue
            val (id, premises, conclusion) =
                when (parts.size) {
                    3 -> Triple(parts[0], premisesOf(parts[1]), parts[2])
                    2 -> Triple(parts[0], emptyList(), parts[1])
                    else -> continue
                }
            if (premises.any { it is Struct && it.arity == 1 && it.functor == "{}" }) constraints = true
            if (boundByQuery(premises, conclusion)) ids += id
        }
        val unifier = Arg2pUnificator.mgu(Tuple.of(second, third), Tuple.of(List.of(ids), Atom.of(if (constraints) "yes" else "no")))
        return if (unifier.isSuccess) sequenceOf(unifier) else emptySequence()
    }

    // The premises of a rule: a list, or a conjunction ([] for none)
    private fun premisesOf(term: Term): kotlin.collections.List<Term> =
        when {
            term is List -> term.toList()
            term is Struct && term.arity == 2 && term.functor == "," -> premisesOf(term[0]) + premisesOf(term[1])
            else -> listOf(term)
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
