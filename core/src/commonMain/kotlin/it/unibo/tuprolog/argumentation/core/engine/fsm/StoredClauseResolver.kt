package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * Resolution of a goal with a clause as stored, for the unificator of arg2p (strict equality of constants, no occurs
 * check): the head of the clause is unified with the goal and the body built from the result, the variables of the
 * clause being renamed on the way. [RenamingClauseResolver] copies the clause as a whole, unifies its head, then
 * prepares its body for execution and builds it with the unifier: here the clause is visited once, and only its
 * parts that are not ground are copied.
 *
 * A variable of the clause meeting a term of the goal stands for that term, and a variable of the goal meeting a
 * term of the clause is bound to its copy: the bindings only bind variables of the goal, kept applied to each other.
 *
 * An anonymous variable (`_`) never stands for another, nor is the value of one: a copy of a term (copy_term/2,
 * findall/3) gives each occurrence of an anonymous variable a variable of its own, which would part the places it
 * stands for (`p(X, X)` met by `_` copied as `p(A, B)`). A named variable takes its place.
 */
internal object StoredClauseResolver : ClauseResolver {
    override fun resolve(
        rule: Rule,
        goal: Term,
    ): Resolution? = Unification().resolve(rule, goal)

    // The state of a single resolution
    private class Unification {
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

        // The bindings of the variables of the goal
        private val goalBindings: Substitution.Unifier
            get() =
                if (fresh.none { it in bindings }) {
                    unifier
                } else {
                    Substitution.unifier(bindings.filterKeys { it !in fresh })
                }

        fun resolve(
            rule: Rule,
            goal: Term,
        ): Resolution? =
            if (unifyWithClause(rule.head, goal)) {
                Resolution(goalBindings, prepare(instantiate(rule.body)))
            } else {
                null
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
                clauseTerm is Var -> renamed[clauseTerm]?.let { unify(it, goal) } ?: true.also { renamed[clauseTerm] = named(goal) }
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
                first is Var && second is Var -> first == second || bindVariables(first, second)
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

        // Two variables of the goal meeting: the left one bound to the right one, but an anonymous one bound to a named one
        private fun bindVariables(
            first: Var,
            second: Var,
        ): Boolean =
            when {
                !second.isAnonymous -> bind(first, second)
                !first.isAnonymous -> bind(second, first)
                else -> named(first).let { bind(second, it) }
            }

        // A term of the goal, or a named variable bound in place of an anonymous one
        private fun named(term: Term): Term =
            if (term is Var && term.isAnonymous) {
                Var.of("_N").also {
                    fresh.add(it)
                    bind(term, it)
                }
            } else {
                term
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
                term is Var -> renamed.getOrPut(term) { Var.of(nameOf(term)).also { fresh.add(it) } }
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
                            ?: Var.of(nameOf(term)).also { renamed[term] = it }
                    }
                term is Struct -> Struct.of(term.functor, List(term.arity) { instantiate(term[it]) })
                else -> term
            }

        // The name of the copy of a variable of the clause: a named one for an anonymous one (see above)
        private fun nameOf(variable: Var): String = if (variable.isAnonymous) "_N" else variable.name

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
    }
}
