package it.unibo.tuprolog.argumentation.core.flags

object QueryMode : ArgsFlag {
    override fun predicate(): String = "queryMode"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true
}
