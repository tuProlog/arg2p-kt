package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.OrderingComparator as OrderingComparatorValue

/** How two sets of rules or premises are compared (see the module superiority) */
object OrderingComparator : ArgsFlag {
    override fun predicate(): String = "orderingComparator"

    override fun default(): String = OrderingComparatorValue.ELITIST.value

    override fun values(): Iterable<String> = OrderingComparatorValue.entries.map { it.value }
}
