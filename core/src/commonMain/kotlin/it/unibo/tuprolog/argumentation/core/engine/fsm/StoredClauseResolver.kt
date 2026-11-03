package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.runtime.isKnownGround
import it.unibo.tuprolog.argumentation.core.runtime.rebuilt
import it.unibo.tuprolog.argumentation.core.runtime.structOf
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.utils.Cursor

/**
 * Resolution of a goal with a clause as stored, for the unificator of arg2p (strict equality of constants, no occurs
 * check): the head of the clause is unified with the goal and the body built from the result, the variables of the
 * clause being renamed on the way. [RenamingClauseResolver] copies the clause as a whole, unifies its head, then
 * prepares its body for execution and builds it with the unifier: here the clause is visited once, and only its
 * parts that are not ground are copied.
 *
 * A variable of the clause meeting a term of the goal stands for that term, and a variable of the goal meeting a
 * term of the clause is bound to its copy: the bindings only bind variables of the goal, applied to each other once
 * the head is unified.
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

    // The state of a single resolution: its maps are allocated when a binding needs them (most resolutions, of facts
    // by ground goals, need none), and the bindings are read through each other and resolved once at the end (see
    // Arg2pUnificator)
    private class Unification {
        // The variables of the clause, as the terms of the goal (or the fresh variables) they stand for; once the
        // head is unified, with the bindings applied as the body is instantiated (see instantiate)
        private var renamed: VarMap? = null

        // The fresh variables, standing for the variables of the clause not met by a term of the goal (each as itself)
        private var fresh: VarMap? = null

        // The bindings of the variables of the goal (and of the fresh ones)
        private var bindings: VarMap? = null

        fun resolve(
            rule: Rule,
            goal: Term,
        ): Resolution? =
            if (unifyWithClause(rule.head, goal)) {
                Resolution(goalBindings(), bodyGoals(rule.body))
            } else {
                null
            }

        // The goals of the body, instantiated one by one (the body as stored flattened, see addGoalsTo): its instance
        // built as a tuple computed the groundness of each fresh goal when built, by sequences over its whole term
        private fun bodyGoals(body: Term): Cursor<out Term> {
            if (body.isTrue) return Cursor.empty()
            val goals = ArrayList<Term>(4)
            addBodyGoals(body, goals)
            return ListCursor.of(goals)
        }

        private fun addBodyGoals(
            body: Term,
            into: ArrayList<Term>,
        ) {
            var current = body
            while (current is Tuple) {
                addBodyGoals(current.left, into)
                current = current.right
            }
            instantiate(current).addGoalsTo(into, prepared = true)
        }

        // The bindings of the variables of the goal, each with the others applied, the fresh variables left out
        private fun goalBindings(): Map<Var, Term> {
            val found = bindings ?: return emptyMap()
            val resolved = VarMap()
            for (i in 0 until found.size) {
                val variable = found.keyAt(i)
                if (fresh?.containsKey(variable) != true) resolved.put(variable, resolve(found.valueAt(i), null))
            }
            return resolved
        }

        private fun rename(
            variable: Var,
            term: Term,
        ) {
            (renamed ?: VarMap().also { renamed = it }).put(variable, term)
        }

        private fun freshVariable(name: String): Var = Var.of(name).also { (fresh ?: VarMap().also { fresh = it }).put(it, it) }

        // The term a variable of the goal stands for, through the variables bound to variables
        private fun dereference(term: Term): Term {
            var current = term
            while (current is Var) current = bindings?.get(current) ?: return current
            return current
        }

        // Unifies a term of the clause with one of the goal
        private fun unifyWithClause(
            clauseTerm: Term,
            goalTerm: Term,
        ): Boolean {
            // (a variable is not asked whether it is ground: it would collect its variables in a sequence to answer)
            if (clauseTerm !is Var && clauseTerm.isGround) return unify(clauseTerm, goalTerm)
            val goal = dereference(goalTerm)
            return when {
                clauseTerm is Var -> renamed?.get(clauseTerm)?.let { unify(it, goal) } ?: true.also { rename(clauseTerm, named(goal)) }
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
                freshVariable("_N").also { bind(term, it) }
            } else {
                term
            }

        private fun bind(
            variable: Var,
            term: Term,
        ): Boolean {
            (bindings ?: VarMap().also { bindings = it }).put(variable, term)
            return true
        }

        // A term of the clause as a term of the goal: its variables renamed as they stand
        private fun copy(term: Term): Term =
            when {
                term is Var -> renamed?.get(term) ?: freshVariable(nameOf(term)).also { rename(term, it) }
                term.isGround -> term
                term is Struct -> structOf(term.functor, List(term.arity) { copy(term[it]) })
                else -> term
            }

        // A term of the clause as a term of the goal, with the bindings applied. The head is unified by now: the term
        // a variable of the clause stands for is kept with the bindings applied, once resolved (a map of the
        // instances was allocated for each resolution)
        private fun instantiate(term: Term): Term =
            when {
                term is Var -> {
                    val known = renamed?.get(term)
                    when {
                        known == null -> Var.of(nameOf(term)).also { rename(term, it) }
                        bindings == null -> known
                        else -> resolve(known, null).also { if (it !== known) renamed!!.put(term, it) }
                    }
                }
                term.isGround -> term
                term is Struct -> structOf(term.functor, List(term.arity) { instantiate(term[it]) })
                else -> term
            }

        // [term], of the goal, with the bindings applied through the chains of bound variables; [path] the variables
        // being resolved, a variable bound to a term holding it (there is no occurs check) left in it. The terms of
        // the goal are the fresh ones of the resolution: the plain compound ones are visited (see isKnownGround)
        private fun resolve(
            term: Term,
            path: ArrayList<Var>?,
        ): Term =
            when {
                term is Var -> {
                    val bound = bindings?.get(term)
                    if (bound == null || (path != null && term in path)) {
                        term
                    } else {
                        val visiting = (path ?: ArrayList(4)).also { it.add(term) }
                        resolve(bound, visiting).also { visiting.removeAt(visiting.size - 1) }
                    }
                }
                term is Struct && !term.isKnownGround() -> term.rebuilt { resolve(it, path) }
                else -> term
            }

        // The name of the copy of a variable of the clause: a named one for an anonymous one (see above)
        private fun nameOf(variable: Var): String = if (variable.isAnonymous) "_N" else variable.name
    }
}
