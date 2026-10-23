package it.unibo.tuprolog.argumentation.core.flags

object ModulesPath : ArgsFlag<String, Unit> {
    override fun predicate(): String = "modulesPath"

    override fun default(): String = "none"

    override fun values() {}
}
