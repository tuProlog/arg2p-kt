package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

data class LabelledArgument(
    val argument: Argument,
    val label: Label,
) {
    override fun toString(): String = "$label(${argument.termRepresentation()})"

    fun toTerm(): Term = Struct.parse(this.toString())
}
