package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.unify.Unificator

/**
 * The unificator of the argumentation solvers: the default one, without the occurs check unless explicitly
 * requested (as in unify_with_occurs_check/2), as in standard Prolog, computed in a single pass (see [Unification]).
 *
 * The occurs check visits the whole term bound to a variable, and arguments (nested lists of rules,
 * conclusions and premises) are bound to variables at almost every resolution step.
 */
object Arg2pUnificator : Unificator by Unificator.default {
    override fun mgu(
        term1: Term,
        term2: Term,
    ): Substitution = Unification().takeIf { it.unify(term1, term2) }?.result ?: Substitution.failed()

    /**
     * The most general unifier of two terms, as the default unificator computes it (strict equality of constants, a
     * variable bound to what it meets, the left one when two variables meet, no occurs check), but in a single
     * recursive pass: the default one rewrites a queue of equations, applying each new binding to all of them.
     * The bindings are kept applied to each other, as in the resulting substitution.
     */
    private class Unification {
        private val bindings = LinkedHashMap<Var, Term>()

        val result: Substitution
            get() = Substitution.unifier(bindings)

        private fun dereference(term: Term): Term = if (term is Var) bindings[term] ?: term else term

        fun unify(
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

        // Two variables meeting: the left one bound to the right one, but an anonymous one (`_`) bound to a named one,
        // and two anonymous ones to a named one: a copy of a term (copy_term/2, findall/3) gives each occurrence of an
        // anonymous variable a variable of its own, which would part the places a binding makes it stand for
        private fun bindVariables(
            first: Var,
            second: Var,
        ): Boolean =
            when {
                !second.isAnonymous -> bind(first, second)
                !first.isAnonymous -> bind(second, first)
                else -> Var.of("_N").let { bind(first, it) && bind(second, it) }
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
    }

    /**
     * Theories look clauses up by matching `Head :- _` against each stored clause: the body is a fresh variable,
     * which matches any body, so only the heads are compared. Matching the bodies too would cost, with the
     * default unificator, a pass over the stored body for every variable of the head.
     */
    override fun match(
        term1: Term,
        term2: Term,
    ): Boolean {
        if (term1 is Rule && term2 is Clause) {
            val body = term1.body
            if (body is Var && term1.head.variables.none { it == body }) {
                return term2.head?.let { Unification().unify(term1.head, it) } ?: false
            }
        }
        return Unification().unify(term1, term2)
    }

    override fun unify(
        term1: Term,
        term2: Term,
    ): Term? = mgu(term1, term2).takeIf { it.isSuccess }?.let { term1.apply(it) }

    // Merging with an empty substitution (as the replies of most primitives, which only test) copies nothing
    override fun merge(
        substitution1: Substitution,
        substitution2: Substitution,
    ): Substitution =
        when {
            substitution2.isSuccess && substitution2.isEmpty() -> substitution1
            substitution1.isSuccess && substitution1.isEmpty() -> substitution2
            else -> Unificator.default.merge(substitution1, substitution2, false)
        }

    override fun merge(
        substitution1: Substitution,
        substitution2: Substitution,
        occurCheckEnabled: Boolean,
    ): Substitution =
        if (occurCheckEnabled) {
            Unificator.default.merge(substitution1, substitution2, true)
        } else {
            merge(substitution1, substitution2)
        }
}
