package it.unibo.tuprolog.argumentation.core.flags

object AmbiguityBlocking : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "ambiguityBlocking"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true

    override fun values() {}
}
