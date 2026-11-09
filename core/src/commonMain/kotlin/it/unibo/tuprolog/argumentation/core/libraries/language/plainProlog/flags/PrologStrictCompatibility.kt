package it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

object PrologStrictCompatibility : ArgsFlag {
    override fun predicate(): String = "prologStrictCompatibility"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false
}
