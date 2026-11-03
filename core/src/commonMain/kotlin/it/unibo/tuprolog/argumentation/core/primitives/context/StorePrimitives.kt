package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.Sourcing
import it.unibo.tuprolog.argumentation.core.engine.attempts
import it.unibo.tuprolog.argumentation.core.engine.items
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.argumentation.core.runtime.context.StoreClauses
import it.unibo.tuprolog.argumentation.core.runtime.context.StoreLookup
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

// The lookups of the primitives of the stores (the branches of the context, the cache), as the machine runs them

/**
 * The lookup of the argument [i] in [store] (see [StoreClauses.delegation]): the goal entered with the clauses of
 * the store, a conjunction as the lookup of each goal, or the answers of the store itself for anything else
 */
internal fun NativeCall.lookup(
    store: ClauseStore,
    lookup: StoreLookup,
    i: Int,
): Int {
    val goal = term(arg(i))
    return when (val delegation = StoreClauses.delegation(store, lookup, goal, libraries)) {
        is Inlining -> inline(word(delegation.goal), delegation.libraries, delegation.theory)
        is Sourcing -> sourced(arg(i), delegation.source)
        null -> answersOf(store.solve(goal).iterator())
    }
}

/** An answer per substitution of [substitutions], the arguments of the call unified under it */
internal fun NativeCall.answersOf(substitutions: Iterator<Substitution>): Int {
    val arguments = List(arity) { term(arg(it)) }
    return attempts(
        substitutions
            .asSequence()
            .map { substitution -> { call: NativeCall -> call.unifiesAll(arguments, substitution) } }
            .iterator(),
    )
}

private fun NativeCall.unifiesAll(
    arguments: List<Term>,
    substitution: Substitution,
): Boolean {
    for (i in arguments.indices) if (!unify(arg(i), word(arguments[i].apply(substitution)))) return false
    return true
}

/** The branches the list argument [i] names (a type error otherwise, from the primitive of [signature]) */
internal fun NativeCall.branchesArg(
    i: Int,
    signature: Signature,
): Set<Int> {
    val items = items(arg(i)) ?: throw TypeError.forArgument(context, signature, TypeError.Expected.LIST, term(arg(i)), i)
    return items
        .map { w: Int ->
            if (kind(w) == NativeCall.INT) {
                intValue(w).toInt()
            } else {
                throw TypeError.forArgument(context, signature, TypeError.Expected.INTEGER, term(w), i)
            }
        }.toSet()
}
