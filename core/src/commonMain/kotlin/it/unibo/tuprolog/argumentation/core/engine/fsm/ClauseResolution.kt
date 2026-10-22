package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * Resolution of a goal with a clause as stored (arg2p): the head of the clause is unified with the goal and the body
 * built from the result, the variables of the clause being renamed on the way. The clause was renamed as a whole
 * (copying it), then its head unified with the goal, then its body prepared for execution and built with the
 * unifier: the clause is now visited once, and only the parts of it that are not ground are copied.
 *
 * The unification is the one of the engine (strict equality of constants, no occurs check): a variable of the clause
 * meeting a term of the goal stands for that term, and a variable of the goal meeting a term of the clause is bound
 * to its copy. The resulting [bindings] only bind variables of the goal, kept applied to each other.
 */
internal class ClauseResolution private constructor() {
    // The variables of the clause, as the terms of the goal (or the fresh variables) they stand for
    private val renamed = HashMap<Var, Term>()

    // The fresh variables, standing for the variables of the clause not met by a term of the goal
    private val fresh = HashSet<Var>()

    // The bindings of the variables of the goal (and of the fresh ones), applied to each other
    private val bindings = LinkedHashMap<Var, Term>()

    private val unifier: Substitution.Unifier by lazy(LazyThreadSafetyMode.NONE) {
        Substitution.unifier(bindings)
    }

    // The variables of the clause in the body, with the bindings applied
    private val instantiated = HashMap<Var, Term>()

    /** The bindings of the variables of the goal */
    val goalBindings: Substitution.Unifier
        get() =
            if (fresh.none { it in bindings }) {
                unifier
            } else {
                Substitution.unifier(bindings.filterKeys { it !in fresh })
            }

    private fun dereference(term: Term): Term = if (term is Var) bindings[term] ?: term else term

    // Unifies a term of the clause with one of the goal
    private fun unifyWithClause(
        clauseTerm: Term,
        goalTerm: Term,
    ): Boolean {
        if (clauseTerm.isGround) return unify(clauseTerm, goalTerm)
        val goal = dereference(goalTerm)
        return when {
            clauseTerm is Var -> renamed[clauseTerm]?.let { unify(it, goal) } ?: true.also { renamed[clauseTerm] = goal }
            goal is Var -> bind(goal, copy(clauseTerm))
            clauseTerm is Struct && goal is Struct ->
                clauseTerm.arity == goal.arity &&
                    clauseTerm.functor == goal.functor &&
                    (0 until clauseTerm.arity).all { unifyWithClause(clauseTerm[it], goal[it]) }
            else -> false
        }
    }

    // Unifies two terms of the goal (as Arg2pUnificator)
    private fun unify(
        term1: Term,
        term2: Term,
    ): Boolean {
        val first = dereference(term1)
        val second = dereference(term2)
        return when {
            first is Var && second is Var -> first == second || bind(first, second)
            first is Var -> bind(first, second)
            second is Var -> bind(second, first)
            first.isConstant || second.isConstant -> first == second
            first is Struct && second is Struct ->
                first.arity == second.arity &&
                    first.functor == second.functor &&
                    (0 until first.arity).all { unify(first[it], second[it]) }
            else -> first == second
        }
    }

    private fun bind(
        variable: Var,
        term: Term,
    ): Boolean {
        val value = if (bindings.isEmpty() || term.isGround) term else term.apply(Substitution.unifier(bindings))
        if (bindings.isNotEmpty()) {
            val binding = Substitution.of(variable, value)
            for (entry in bindings.entries) {
                if (!entry.value.isGround) entry.setValue(entry.value.apply(binding))
            }
        }
        bindings[variable] = value
        return true
    }

    // A term of the clause as a term of the goal: its variables renamed as they stand
    private fun copy(term: Term): Term =
        when {
            term.isGround -> term
            term is Var -> renamed.getOrPut(term) { Var.of(term.name).also { fresh.add(it) } }
            term is Struct -> Struct.of(term.functor, List(term.arity) { copy(term[it]) })
            else -> term
        }

    // A term of the clause as a term of the goal, with the bindings applied
    private fun instantiate(term: Term): Term =
        when {
            term.isGround -> term
            term is Var ->
                instantiated.getOrPut(term) {
                    renamed[term]?.let { if (bindings.isEmpty()) it else it.apply(unifier) }
                        ?: Var.of(term.name).also { renamed[term] = it }
                }
            term is Struct -> Struct.of(term.functor, List(term.arity) { instantiate(term[it]) })
            else -> term
        }

    // As Clause.prepareForExecution: the variables in the place of a goal are called through call/1
    private fun prepare(goal: Term): Term =
        when {
            goal is Var -> Struct.of("call", goal)
            goal is Struct && goal.arity == 2 && goal.functor in Clause.notableFunctors -> {
                val left = prepare(goal[0])
                val right = prepare(goal[1])
                if (left === goal[0] && right === goal[1]) goal else Struct.of(goal.functor, left, right)
            }
            else -> goal
        }

    companion object {
        /**
         * The resolution of [goal] with [rule] (as stored, not renamed): the body to execute, with the bindings of
         * the variables of the goal; null if the head of the rule does not unify with the goal
         */
        fun of(
            rule: Rule,
            goal: Term,
        ): Pair<Substitution.Unifier, Term>? {
            val resolution = ClauseResolution()
            if (!resolution.unifyWithClause(rule.head, goal)) return null
            val body = resolution.prepare(resolution.instantiate(rule.body))
            return resolution.goalBindings to body
        }
    }
}
