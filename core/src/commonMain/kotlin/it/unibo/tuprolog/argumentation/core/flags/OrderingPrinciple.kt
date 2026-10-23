package it.unibo.tuprolog.argumentation.core.flags

/** Which rules and premises of two arguments their comparison weighs (see the module superiority) */
object OrderingPrinciple : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingPrinciple"

    override fun default(): String = "last"

    override fun values(): Iterable<String> = listOf("last", "weakest")
}
