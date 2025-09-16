package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

data class Support(
    val supporter: Argument,
    val supported: Argument,
) {
    override fun toString(): String = "support(${supporter.termRepresentation()}, ${supported.termRepresentation()})"

    fun toTerm(): Term = Struct.parse(this.toString())
}
