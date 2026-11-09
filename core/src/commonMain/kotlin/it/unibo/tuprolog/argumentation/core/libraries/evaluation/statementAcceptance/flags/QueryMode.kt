package it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

object QueryMode : ArgsFlag {
    override fun predicate(): String = "queryMode"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true
}
