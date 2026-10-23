package it.unibo.tuprolog.argumentation.core.flags

object PrologStrictCompatibility : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "prologStrictCompatibility"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
