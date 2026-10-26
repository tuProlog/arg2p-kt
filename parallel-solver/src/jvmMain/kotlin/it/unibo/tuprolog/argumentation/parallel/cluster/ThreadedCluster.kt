package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.federation.PoolScheduler
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import java.util.concurrent.CompletableFuture

/**
 * A distributed solver on the threads of this JVM: each node (the catalog, each worker) handles its messages one
 * at a time, the nodes in parallel on [threads] threads. The theory is split into at most [parts] partitions if
 * given, as many as the workers of the cluster and of the sites it is federated with otherwise (see [site]); the
 * graph is labelled as the standard solver does with [settings]. The nodes share the terms of their messages; with
 * [textual], every message is written as text by its sender and read by its receiver instead, as between processes
 * (see [it.unibo.tuprolog.argumentation.parallel.message.MessageCodec]).
 */
class ThreadedCluster(
    threads: Int = Runtime.getRuntime().availableProcessors(),
    parts: Int? = null,
    settings: FlagsBuilder = FlagsBuilder(),
    textual: Boolean = false,
    lends: Boolean = true,
) : AutoCloseable {
    private val scheduler = PoolScheduler(threads)

    /** The site of the cluster, running a worker on each thread: to federate it with others, through its links */
    val site = Site(scheduler, threads, parts, settings, lends, textual)

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = site.load(theory)

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = site.add(clause)

    /** Forgets the theory */
    fun reset() = site.reset()

    /** The arguments for [statement], as Prolog text, by label: the evaluations of several callers run in parallel */
    fun solve(statement: String): EvaluationResponse =
        CompletableFuture<EvaluationResponse>()
            .also { response -> site.solve(statement) { response.complete(it) } }
            .join()

    /** Stops the threads */
    override fun close() = scheduler.close()
}
