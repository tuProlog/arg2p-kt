package it.unibo.tuprolog.argumentation.engine.term

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var

/**
 * The variables of a term, collected by a plain recursion (Term.variables nests a lazy sequence for each argument),
 * skipping its parts ground at hand (see [isKnownGround]: the goals are fresh terms, whose groundness a plain compound
 * term computed by sequences over its whole subterm)
 */
internal fun Term.collectVariables(into: MutableSet<Var>) {
    when (this) {
        is Var -> into.add(this)
        is Struct -> if (!isKnownGround()) for (i in 0 until arity) this[i].collectVariables(into)
        else -> Unit
    }
}

/**
 * Whether this term is ground, where the answer is at hand (arg2p): a list or a tuple knows it when built, an atom or
 * a number always, a variable never. A plain compound term computes it on its first request, by sequences over its
 * whole subterm allocated at every level, which a walk of the term costs less than: it visits its parts instead
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
