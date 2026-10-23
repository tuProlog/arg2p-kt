@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.causality.js

import it.unibo.tuprolog.argumentation.causality.modules.CausalityModule
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import kotlin.js.JsExport

/**
 * The causality solver for JS: its module (causality::ness/3, ...), to give the solvers that need it
 * (`new JsSolverBuilder().module(JsCausality.module())`)
 */
@JsExport
object JsCausality {
    /** A new causality module: each solver has its own (it keeps the graphs it computes) */
    fun module(): ArgLibrary = CausalityModule()
}
