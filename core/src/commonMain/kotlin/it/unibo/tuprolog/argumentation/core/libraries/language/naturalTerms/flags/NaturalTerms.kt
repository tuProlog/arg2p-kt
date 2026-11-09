package it.unibo.tuprolog.argumentation.core.libraries.language.naturalTerms.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

// Read from the text of a theory alone (`naturalTerms.`): the statements are written as text
object NaturalTerms : ArgsFlag {
    override fun predicate(): String = "naturalTerms"

    override val isSwitch: Boolean = true

    override val isInternal: Boolean = true

    override fun default(): Boolean = false
}
