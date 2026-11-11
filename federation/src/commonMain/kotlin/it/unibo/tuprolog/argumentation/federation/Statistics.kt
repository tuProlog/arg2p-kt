@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.ExperimentalArg2pApi
import kotlin.js.JsExport
import kotlin.js.JsName

/** A federation, as one of its sites ([site]) knows it: the [sites] it knows, itself first */
@JsExport
@ExperimentalArg2pApi
data class FederationStats(
    val site: String,
    @JsExport.Ignore @JsName("sitesList") val sites: List<SiteStats>,
) {
    /** The sites known, itself first (an array in JavaScript) */
    @JsName("sites")
    val siteArray: Array<SiteStats> get() = sites.toTypedArray()
}

/** A site of a federation: its identifier, its [name] (empty if it has none, as a browser), and the consumers it offers (null for no limit) */
@JsExport
@ExperimentalArg2pApi
data class SiteStats(
    val id: String,
    val name: String,
    val workers: Int?,
)
