@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.model.terms.LabelTerm
import it.unibo.tuprolog.core.Struct
import kotlin.js.JsExport

/** An argument with its [label]: in, out, und (na for an argument the labelling does not label) */
@JsExport
data class LabelledArgument(
    val argument: Argument,
    val label: Label,
) {
    override fun toString(): String = toTerm().toString()

    /** The fact labelling the argument (see [LabelTerm]) */
    @JsExport.Ignore
    fun toTerm(): Struct = LabelTerm.argument(label, argument.termRepresentation())
}
