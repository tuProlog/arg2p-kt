package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.OrderingPrinciple as OrderingPrincipleValue

/** Which rules and premises of two arguments their comparison weighs (see the module superiority) */
object OrderingPrinciple : ArgsFlag {
    override fun predicate(): String = "orderingPrinciple"

    override fun default(): String = OrderingPrincipleValue.LAST.value

    override fun values(): Iterable<String> = OrderingPrincipleValue.entries.map { it.value }
}
