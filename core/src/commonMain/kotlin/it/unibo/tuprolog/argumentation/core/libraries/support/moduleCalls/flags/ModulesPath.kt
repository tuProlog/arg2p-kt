package it.unibo.tuprolog.argumentation.core.libraries.support.moduleCalls.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

object ModulesPath : ArgsFlag {
    override fun predicate(): String = "modulesPath"

    override fun default(): String = "none"

    override val isOpen: Boolean = true
}
