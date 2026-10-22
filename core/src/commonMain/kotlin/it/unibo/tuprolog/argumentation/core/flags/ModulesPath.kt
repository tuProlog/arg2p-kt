package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag

object ModulesPath : ArgsFlag<String, Unit> {
    override fun predicate(): String = "modulesPath"

    override fun default(): String = "none"

    override fun values() {}
}
