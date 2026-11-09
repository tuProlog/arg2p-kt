package it.unibo.tuprolog.argumentation.core.libraries.language.transposition.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

object AutoTransposition : ArgsFlag {
    override fun predicate(): String = "autoTransposition"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false
}
