package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag

// The flags of the language of the theories, declared by the parser

object AutoTransposition : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "autoTransposition"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}

object PrologStrictCompatibility : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "prologStrictCompatibility"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}

// Conflicts derived as statements of the theory (conflict/2,3 arguments), rather than built in
object MetaConflicts : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "metaConflicts"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}

// Rules applicable only when an applicable(Rule) statement holds
object MetaRules : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "metaRules"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
