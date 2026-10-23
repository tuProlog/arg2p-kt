package it.unibo.tuprolog.argumentation.core.flags

object AutoTransposition : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "autoTransposition"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
