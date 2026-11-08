package it.unibo.tuprolog.argumentation.core.flags

object ModulesPath : ArgsFlag {
    override fun predicate(): String = "modulesPath"

    override fun default(): String = "none"

    override val isOpen: Boolean = true
}
