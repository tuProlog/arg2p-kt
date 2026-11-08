package it.unibo.tuprolog.argumentation.core.flags

// Rules applicable only when an applicable(Rule) statement holds
object MetaRules : ArgsFlag {
    override fun predicate(): String = "metaRules"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false
}
