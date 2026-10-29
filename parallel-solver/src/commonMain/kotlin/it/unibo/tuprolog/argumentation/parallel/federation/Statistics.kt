@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.js.JsExport
import kotlin.js.JsName

/**
 * What a federation does, as one of its sites ([site]) knows it: the [sites] it knows, itself first, the last
 * evaluation it answered ([evaluation], none before the first), and the sites heard on the local network that are not
 * its peers ([nearby], see [Beacons])
 */
@JsExport
data class FederationStats(
    val site: String,
    @JsExport.Ignore @JsName("sitesList") val sites: List<SiteStats>,
    val evaluation: EvaluationStats?,
    @JsExport.Ignore @JsName("nearbyList") val nearby: List<NearbySite> = emptyList(),
) {
    /** The sites known, itself first (an array in JavaScript) */
    @JsName("sites")
    val siteArray: Array<SiteStats> get() = sites.toTypedArray()

    /** The sites heard nearby, not peers (an array in JavaScript) */
    @JsName("nearby")
    val nearbyArray: Array<NearbySite> get() = nearby.toTypedArray()
}

/**
 * A site heard on the local network (see [Beacons]): its identifier, its [name] (the name of its host, or the one it
 * was given), its [address] (ws://host:port) and whether it requires a code of the sites linking to it
 * ([requiresCode]: it waits to be paired)
 */
@JsExport
data class NearbySite(
    val id: String,
    val name: String,
    val address: String,
    val requiresCode: Boolean,
)

/**
 * A site of a federation: its identifier, the workers it offers (its delegates' included, for the site knowing it as
 * for the others; null for no limit, or for a site reached through another, whose workers are not known), whether it
 * is a [delegate] of the site knowing it (a thread or a Web Worker of it, listed as well, with its own workers: the
 * workers of a federation are those of the sites that are not delegates), and the site through which it is reached
 * ([via]: null for the site itself and those linked to it)
 */
@JsExport
data class SiteStats(
    val id: String,
    val workers: Int?,
    val delegate: Boolean,
    val via: String?,
)

/**
 * An evaluation answered by the catalog of a site (a statement evaluated, or a goal labelling or reading a graph): the
 * part of the graph it was answered on ([locality]: directional or connected in query mode, whole_graph otherwise, or
 * catalog for a graph the catalog builds alone, as an abstract framework, with no worker), the
 * arguments and the attacks of the graph labelled (those the catalog derives from the others included), the arguments
 * the workers built for it ([added]: all of them for a graph built anew, those explored further in query mode, none
 * for an evaluation answered on the graph as it was), what each worker of the graph built ([workers], one for each
 * partition of the theory), and how long the evaluation took, in milliseconds (the construction of the graph included)
 */
@JsExport
data class EvaluationStats(
    val locality: String,
    val arguments: Int,
    val attacks: Int,
    val added: Int,
    @JsExport.Ignore @JsName("workersList") val workers: List<WorkerStats>,
    val milliseconds: Double,
) {
    /** The workers of the graph, by partition (an array in JavaScript) */
    @JsName("workers")
    val workerArray: Array<WorkerStats> get() = workers.toTypedArray()
}

/**
 * A worker of a graph: its [partition] of the theory, the [site] that ran it (as the site of the catalog placed it: a
 * site lending the workers of its delegates runs them on those), the arguments of its rules it built and the direct
 * attacks on them it found
 */
@JsExport
data class WorkerStats(
    val partition: Int,
    val site: String,
    val arguments: Int,
    val attacks: Int,
)
