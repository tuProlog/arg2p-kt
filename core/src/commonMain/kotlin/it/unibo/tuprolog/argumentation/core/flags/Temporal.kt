package it.unibo.tuprolog.argumentation.core.flags

// The temporal axioms in the theory (module temporal), with the statements over regions of the extension constraints
object Temporal : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "temporal"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
