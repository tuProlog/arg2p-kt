@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.federation.FederationStats
import it.unibo.tuprolog.argumentation.parallel.federation.NearbySite
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import kotlin.js.JsExport

/**
 * A distributed solver, on any platform (see [LocalCluster], and the clusters of the threads of each platform): the
 * theory and the flags of its site, and what its federation does
 */
@JsExport
abstract class Cluster internal constructor(
    /** The site of the cluster, to federate it with others, through its links */
    @property:JsExport.Ignore val site: Site,
) {
    /**
     * The flags of the solver, the initial ones those given: those set apply from the next evaluation on, the graph
     * built anew, the links and the peers of the cluster kept
     */
    var flags: Flags
        get() = site.flags
        set(value) {
            site.flags = value
        }

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = site.load(theory)

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = site.add(clause)

    /** Forgets the theory */
    fun reset() = site.reset()

    /** What the federation does, as the cluster knows it: the sites, and the last evaluation it answered (see [Site.stats]) */
    val stats: FederationStats get() = site.stats
}

/** A cluster linked to other sites by transports of its own (WebSockets, the messages between threads, WebRTC) */
@JsExport
abstract class LinkedCluster internal constructor(
    site: Site,
) : Cluster(site) {
    /** Called whenever a site joins or leaves the peers of the cluster (its threads included), on the thread that saw it */
    var onPeersChanged: (() -> Unit)?
        get() = site.onPeersChanged
        set(value) {
            site.onPeersChanged = value
        }
}

/**
 * A cluster finding the clusters of its local network (see [it.unibo.tuprolog.argumentation.parallel.federation.Beacons]),
 * linked to them as they come, or once paired by hand if one of the two requires a code
 */
@JsExport
abstract class DiscoveringCluster internal constructor(
    site: Site,
) : LinkedCluster(site) {
    /** The name of the site of the cluster, as its beacons tell the sites nearby: the name of its host by default */
    var name: String
        get() = site.name
        set(value) {
            site.name = value
        }

    /**
     * Whether the sites linking to this cluster must prove they know its [code], or the token of a pairing made with
     * it: refused otherwise, and after a few wrong codes from an address its codes are refused for a while
     */
    var requiresCode: Boolean
        get() = site.requiresCode
        set(value) {
            site.requiresCode = value
        }

    /**
     * The code the sites linking to this cluster prove they know, if it requires one: four digits, random, renewed
     * once a site pairs with the cluster
     */
    val code: String get() = site.code

    /** A new [code], the one before refused from now on (the pairings made stay) */
    fun newCode(): String = site.newCode()

    /** Called whenever the sites heard on the local network change */
    var onNearbyChanged: (() -> Unit)?
        get() = site.onNearbyChanged
        set(value) {
            site.onNearbyChanged = value
        }

    // The site nearby that is [site], for a pairing: an Arg2pException if none
    internal fun nearbySite(site: String): NearbySite =
        this.site.nearby.find { it.id == site } ?: throw Arg2pException("No site $site nearby")
}
