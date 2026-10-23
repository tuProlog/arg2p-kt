package it.unibo.tuprolog.argumentation.core.js

import kotlin.js.JsExport

/**
 * An attack of a graph, by the identifiers of its arguments: its [type] (rebut, undermine, undercut, ...) and the
 * sub-argument of [to] it is [on], when known
 */
@JsExport
data class JsAttack(
    val from: String,
    val to: String,
    val type: String? = null,
    val on: String? = null,
)
