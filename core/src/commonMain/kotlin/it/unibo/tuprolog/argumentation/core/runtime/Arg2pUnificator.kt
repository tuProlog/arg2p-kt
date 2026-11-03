package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.argumentation.core.engine.fsm.VarMap
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.unify.Unificator

/**
 * Whether this term is ground, where the answer is at hand (arg2p): a list or a tuple knows it when built, an atom or
 * a number always, a variable never. A plain compound term computes it on its first request, by sequences over its
 * whole subterm allocated at every level, which the walks of the engine cost less than: they visit its parts instead
 * (those ground at hand, lists for the most part, visited no further).
 */
internal fun Term.isKnownGround(): Boolean =
    when (this) {
        is Var -> false
        is Cons, is Tuple -> isGround
        is Struct -> arity == 0
        else -> true
    }

/**
 * The compound term of [functor] and [arguments], as `Struct.of` builds it (a list, a tuple, a clause, an indicator,
 * a block, an atom or a plain compound term by its functor and arity), built directly where [Struct.of] copied the
 * arguments on the way (a tuple sliced them)
 */
internal fun structOf(
    functor: String,
    arguments: List<Term>,
): Struct =
    when {
        arguments.size != 2 -> Struct.of(functor, arguments)
        functor == "." -> Cons.of(arguments[0], arguments[1])
        functor == "," -> Tuple.of(arguments[0], arguments[1])
        else -> Struct.of(functor, arguments)
    }

/**
 * This compound term with [part] applied to each argument: the term itself where none changes, built only where one
 * does (arg2p: a list of arguments allocated for every term visited, changed or not, was a tenth of the allocations)
 */
internal inline fun Struct.rebuilt(part: (Term) -> Term): Term {
    var arguments: ArrayList<Term>? = null
    for (i in 0 until arity) {
        val argument = this[i]
        val result = part(argument)
        if (arguments == null && result !== argument) {
            arguments = ArrayList(arity)
            for (j in 0 until i) arguments.add(this[j])
        }
        arguments?.add(result)
    }
    return if (arguments == null) this else structOf(functor, arguments)
}

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
     * The bindings are kept as they are found, each read through the others, and applied to each other once at the
     * end (a substitution built at every binding, applied to the others, made a unification quadratic).
     */
    private class Unification {
        // The bindings found so far, each variable to the term it met (a bound variable among them, followed on
        // reading, see dereference): none until one is needed, as a goal matched against the heads of its clauses
        // binds nothing most of the time
        private var bindings: VarMap? = null

        /** The unifier: each binding with the others applied, so that applying it once applies them all */
        val result: Substitution
            get() {
                val found = bindings ?: return Substitution.empty()
                val resolved = VarMap()
                for (i in 0 until found.size) resolved.put(found.keyAt(i), resolve(found.valueAt(i), null))
                return Substitution.unifier(resolved)
            }

        // The term a variable stands for, through the variables bound to variables
        private fun dereference(term: Term): Term {
            var current = term
            while (current is Var) current = bindings?.get(current) ?: return current
            return current
        }

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
            (bindings ?: VarMap().also { bindings = it }).put(variable, term)
            return true
        }

        // [term] with the bindings applied, through the chains of bound variables; [path] the variables being
        // resolved, a variable bound to a term holding it (there is no occurs check) left in it
        private fun resolve(
            term: Term,
            path: ArrayList<Var>?,
        ): Term =
            when {
                term is Var -> {
                    val bound = bindings!![term]
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
