@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.model.terms.SupportTerm
import it.unibo.tuprolog.core.Struct
import kotlin.js.JsExport

/** A support of a graph: [supporter] is an argument [supported] is built on */
@JsExport
data class Support(
    val supporter: Argument,
    val supported: Argument,
) {
    override fun toString(): String = toTerm().toString()

    /** The term of the support (see [SupportTerm]) */
    @JsExport.Ignore
    fun toTerm(): Struct = SupportTerm.of(supporter.termRepresentation(), supported.termRepresentation())
}
