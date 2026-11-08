package it.unibo.tuprolog.argumentation.core.flags

// The temporal axioms in the theory (module temporal), with the statements over regions of the extension constraints
object Temporal : ArgsFlag {
    override fun predicate(): String = "temporal"

    override val isSwitch: Boolean = true

    // (not among the flags the API tells of, as it may change: a theory states it)
    override val isInternal: Boolean = true

    override fun default(): Boolean = false
}
