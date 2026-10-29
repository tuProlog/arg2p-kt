package it.unibo.tuprolog.argumentation.core.flags

/**
 * The conflicts between statements beyond those of the languages of the theory (see the module conflict), as the
 * parts stated, any of them: negation (a literal and its negation), deontic (the obligations and permissions in
 * conflict), preference (a superiority and its converse) and declared (those the theory declares, conflict/2,3). All
 * of them by default; builtin stands for all of them, and declared alone keeps the declared ones, as in ABA, whose
 * contraries are all declared. `conflictModel(none)` states none of them: the conflicts of the languages alone.
 */
object ConflictModel : ArgsFlag<Iterable<String>, Iterable<String>> {
    /** The parts of the conflicts, in the order they are told */
    val parts: List<String> = listOf("negation", "deontic", "preference", "declared")

    override fun predicate(): String = "conflictModel"

    override fun default(): Iterable<String> = parts

    override fun values(): Iterable<String> = parts + "builtin"

    override val aliases: Map<String, List<String>> = mapOf("builtin" to parts)
}
