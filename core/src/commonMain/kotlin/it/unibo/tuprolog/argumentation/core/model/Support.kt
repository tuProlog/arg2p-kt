package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.model.terms.SupportTerm
import it.unibo.tuprolog.core.Struct

data class Support(
    val supporter: Argument,
    val supported: Argument,
) {
    override fun toString(): String = toTerm().toString()

    /** The term of the support (see [SupportTerm]) */
    fun toTerm(): Struct = SupportTerm.of(supporter.termRepresentation(), supported.termRepresentation())
}
