package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.libs.data.LabelTerm
import it.unibo.tuprolog.core.Struct

data class LabelledArgument(
    val argument: Argument,
    val label: Label,
) {
    override fun toString(): String = toTerm().toString()

    /** The fact labelling the argument (see [LabelTerm]) */
    fun toTerm(): Struct = LabelTerm.argument(label, argument.termRepresentation())
}
