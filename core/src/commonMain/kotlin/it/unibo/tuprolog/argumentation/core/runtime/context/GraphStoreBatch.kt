package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * The graph store (see the module graphStore) written from Kotlin a batch at a time, for the callers saving many
 * arguments, supports or attacks at once: the same facts its saving predicates assert, in the same order, without
 * solving a goal for each. Unlike those predicates, it does not check what is saved already: the caller saves each
 * argument, support or attack once.
 */
object GraphStoreBatch {
    /** The [arguments] ([Rules, TopRule, Conclusion, Body, Info] terms) saved under [tag], as graphStore::saveArgument/3 */
    fun saveArguments(
        context: Context,
        tag: Term,
        arguments: Iterable<Term>,
    ) = arguments.forEach { argument ->
        val parts = argument.castToList().toList()
        val conclusion = parts[2]
        val info = parts[4]
        val stored = Struct.of("argument", argument)
        val id = Integer.of(context.identifier(stored))
        context.assertAll(
            listOf(
                Struct.of("newConc", tag, conclusion),
                stored,
                Rule.of(Struct.of("conc", conclusion), stored),
                Rule.of(Struct.of("arg", id), stored),
                Struct.of("argInfo", id, Integer.of(context.identifier(Struct.of("info", info))), info),
            ),
        )
    }

    /** The [supports] (supporter to supported) saved, as graphStore::saveSupport/2 */
    fun saveSupports(
        context: Context,
        supports: Iterable<Pair<Term, Term>>,
    ) = supports.forEach { (supporter, supported) ->
        context.assertAll(
            listOf(
                Struct.of("support", supporter, supported),
                Struct.of("supportId", id(context, supporter), id(context, supported)),
            ),
        )
    }

    /** An attack of [type] of [attacker] on [attacked], on its sub-argument [on] ([attacked] itself for a direct one) */
    data class Attack(
        val type: Term,
        val attacker: Term,
        val attacked: Term,
        val on: Term,
    )

    /** The [attacks] saved, as graphStore::saveAttack/4 */
    fun saveAttacks(
        context: Context,
        attacks: Iterable<Attack>,
    ) = attacks.forEach { (type, attacker, attacked, on) ->
        context.assertAll(
            listOf(
                Struct.of("attackId", type, id(context, attacker), id(context, attacked), id(context, on)),
                Struct.of("attack", type, attacker, attacked, on),
            ),
        )
    }

    private fun id(
        context: Context,
        argument: Term,
    ) = Integer.of(context.identifier(Struct.of("argument", argument)))
}
