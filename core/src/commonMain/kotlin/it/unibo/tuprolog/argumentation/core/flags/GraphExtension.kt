package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.GraphExtension as GraphExtensionValue

object GraphExtension : ArgsFlag {
    override fun predicate(): String = "graphExtension"

    override fun default(): Iterable<String> = listOf(GraphExtensionValue.STANDARD_PREF.value)

    override val isOpen: Boolean = true

    override fun values(): Iterable<String> = GraphExtensionValue.entries.map { it.value }
}
