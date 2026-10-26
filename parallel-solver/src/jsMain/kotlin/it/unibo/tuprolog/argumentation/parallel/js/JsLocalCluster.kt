package it.unibo.tuprolog.argumentation.parallel.js

import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import kotlin.js.JsExport

/**
 * A distributed solver whose nodes run in turn on the calling thread (see [LocalCluster]): on every JavaScript
 * runtime, browsers included. The theory is split into at most [parts] partitions if given.
 */
@JsExport
class JsLocalCluster(
    parts: Int? = null,
) {
    private val cluster = LocalCluster(parts)

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = cluster.load(theory)

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = cluster.add(clause)

    /** Forgets the theory */
    fun reset() = cluster.reset()

    /** The arguments for [statement], as Prolog text, by label */
    fun solve(statement: String): JsEvaluation = JsEvaluation.of(cluster.solve(statement))
}
