package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.unify.Unificator

/**
 * The unificator of the argumentation solvers: the default one, without the occurs check unless explicitly
 * requested (as in unify_with_occurs_check/2), as in standard Prolog.
 *
 * The occurs check visits the whole term bound to a variable, and arguments (nested lists of rules,
 * conclusions and premises) are bound to variables at almost every resolution step.
 */
object Arg2pUnificator : Unificator by Unificator.default {
    override fun mgu(
        term1: Term,
        term2: Term,
    ): Substitution = Unificator.default.mgu(term1, term2, false)

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
                return term2.head?.let { Unificator.default.match(term1.head, it, false) } ?: false
            }
        }
        return Unificator.default.match(term1, term2, false)
    }

    override fun unify(
        term1: Term,
        term2: Term,
    ): Term? = Unificator.default.unify(term1, term2, false)

    override fun merge(
        substitution1: Substitution,
        substitution2: Substitution,
    ): Substitution = Unificator.default.merge(substitution1, substitution2, false)
}
