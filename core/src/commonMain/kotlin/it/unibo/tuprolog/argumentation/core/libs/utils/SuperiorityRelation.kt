package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class SuperiorityRelationBase : PrologModule("prolog.argumentation.superiority", "superiority") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(OrderingPrinciple, OrderingComparator)
}

expect object SuperiorityRelation : SuperiorityRelationBase {
    override val prologRawTheory: String
}

object OrderingPrinciple : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingPrinciple"

    override fun default(): String = "last"

    override fun values(): Iterable<String> = listOf("last", "weakest")
}

object OrderingComparator : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingComparator"

    override fun default(): String = "elitist"

    override fun values(): Iterable<String> = listOf("elitist", "democrat", "normal")
}
