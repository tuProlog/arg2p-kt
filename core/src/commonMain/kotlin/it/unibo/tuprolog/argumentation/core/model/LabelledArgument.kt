package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

data class LabelledArgument(val argument: Argument, val label: Label) {
    override fun toString(): String {
        return "$label(${argument.termRepresentation()})"
    }

    fun toTerm(): Term {
        return Struct.parse(this.toString())
    }
}
