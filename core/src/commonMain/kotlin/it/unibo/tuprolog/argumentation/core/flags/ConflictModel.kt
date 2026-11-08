package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.ConflictModel as ConflictModelValue

/**
 * The conflicts between statements beyond those of the languages of the theory (see the module conflict), as the
 * parts stated, any of them: negation (a literal and its negation), deontic (the obligations and permissions in
 * conflict), preference (a superiority and its converse) and declared (those the theory declares, conflict/2,3). All
 * of them by default; builtin stands for all of them, and declared alone keeps the declared ones, as in ABA, whose
 * contraries are all declared. `conflictModel(none)` states none of them: the conflicts of the languages alone.
 */
object ConflictModel : ArgsFlag {
    /** The parts of the conflicts, in the order they are told */
    val parts: List<String> = ConflictModelValue.entries.filter { it != ConflictModelValue.BUILTIN }.map { it.value }

    override fun predicate(): String = "conflictModel"

    override fun default(): Iterable<String> = parts

    override fun values(): Iterable<String> = ConflictModelValue.entries.map { it.value }

    override val aliases: Map<String, List<String>> = mapOf(ConflictModelValue.BUILTIN.value to parts)
}
