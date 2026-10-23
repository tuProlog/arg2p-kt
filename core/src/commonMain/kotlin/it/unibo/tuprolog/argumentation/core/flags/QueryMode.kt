package it.unibo.tuprolog.argumentation.core.flags

object QueryMode : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "queryMode"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true

    override fun values() {}
}
