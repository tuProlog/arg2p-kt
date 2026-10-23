package it.unibo.tuprolog.argumentation.core.flags

/** How two sets of rules or premises are compared (see the module superiority) */
object OrderingComparator : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingComparator"

    override fun default(): String = "elitist"

    override fun values(): Iterable<String> = listOf("elitist", "democrat", "normal")
}
