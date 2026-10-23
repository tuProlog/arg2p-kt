package it.unibo.tuprolog.argumentation.core.js

import kotlin.js.JsExport

/** An argument of a graph: its identifier (A0, A1, ...), how it is made (`A2 : A0,r1 : c`), and its label */
@JsExport
data class JsArgument(
    val id: String,
    val descriptor: String,
    val label: String,
)
