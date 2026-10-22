package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag

/** Which rules and premises of two arguments their comparison weighs (see the module superiority) */
object OrderingPrinciple : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingPrinciple"

    override fun default(): String = "last"

    override fun values(): Iterable<String> = listOf("last", "weakest")
}

/** How two sets of rules or premises are compared (see the module superiority) */
object OrderingComparator : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingComparator"

    override fun default(): String = "elitist"

    override fun values(): Iterable<String> = listOf("elitist", "democrat", "normal")
}
