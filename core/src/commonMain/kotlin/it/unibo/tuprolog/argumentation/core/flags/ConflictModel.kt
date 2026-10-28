package it.unibo.tuprolog.argumentation.core.flags

/**
 * The conflicts between statements beyond those the theory declares (see the module conflict): the built-in ones as
 * well (builtin: a literal and its negation, obligations and permissions), or none (declared, as in ABA, whose
 * contraries are all declared)
 */
object ConflictModel : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "conflictModel"

    override fun default(): String = "builtin"

    override fun values(): Iterable<String> = listOf("builtin", "declared")
}
