package it.unibo.tuprolog.argumentation.core.js

import kotlin.js.JsExport

/** An attack of a graph, by the identifiers of its arguments */
@JsExport
data class JsAttack(
    val from: String,
    val to: String,
)
