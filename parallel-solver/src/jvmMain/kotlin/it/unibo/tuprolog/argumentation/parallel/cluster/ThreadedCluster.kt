package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.node.Catalog
import it.unibo.tuprolog.argumentation.parallel.node.Network
import it.unibo.tuprolog.argumentation.parallel.node.Node
import it.unibo.tuprolog.argumentation.parallel.node.Worker
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger

/**
 * A distributed solver on the threads of this JVM: each node (the catalog, each worker) handles its messages one
 * at a time, the nodes in parallel on [threads] threads. The theory is split into at most [parts] partitions (by
 * default as many as the threads), each with a solver of its own; the graph is labelled as the standard solver does
 * with [settings]. The nodes share the terms of the messages; with [textual], every message is written as text by
 * its sender and read by its receiver instead, as between processes (see
 * [it.unibo.tuprolog.argumentation.parallel.message.MessageCodec]).
 */
class ThreadedCluster(
    threads: Int = Runtime.getRuntime().availableProcessors(),
    parts: Int? = threads,
    settings: FlagsBuilder = FlagsBuilder(),
    private val textual: Boolean = false,
) : AutoCloseable {
    private val executor: ExecutorService = Executors.newFixedThreadPool(threads)
    private val network = Network { to, message -> deliver(to, message) }
    private val mailboxes = ConcurrentHashMap<String, Mailbox>()
    private val clients = ConcurrentHashMap<String, CompletableFuture<EvaluationResponse>>()
    private val clientIds = AtomicInteger()

    init {
        mailboxes[Address.CATALOG] = Mailbox(Catalog(network, parts, settings))
    }

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = deliver(Address.CATALOG, Load(theory))

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = deliver(Address.CATALOG, Add(clause))

    /** Forgets the theory */
    fun reset() = deliver(Address.CATALOG, Reset)

    /** The arguments for [statement], as Prolog text, by label: the evaluations of several callers run in parallel */
    fun solve(statement: String): EvaluationResponse {
        val client = "client_${clientIds.incrementAndGet()}"
        val response = CompletableFuture<EvaluationResponse>()
        clients[client] = response
        deliver(Address.CATALOG, RequireEvaluation(statement, client))
        return response.join()
    }

    /** Stops the threads */
    override fun close() {
        executor.shutdownNow()
    }

    private fun deliver(
        to: String,
        message: KbMessage,
    ) {
        if (message is EvaluationResponse) {
            clients.remove(to)?.complete(message)
            return
        }
        // written as text by the sender, read by the receiver, as across processes (see MessageCodec)
        mailboxes.computeIfAbsent(to) { Mailbox(Worker(it, network)) }.post(if (textual) MessageCodec.encode(message) else message)
    }

    // The messages of a node, handled one at a time on the threads of the cluster, in the order they came
    private inner class Mailbox(
        private val node: Node,
    ) {
        private val messages = ConcurrentLinkedQueue<Any>()
        private val scheduled = AtomicBoolean(false)

        fun post(message: Any) {
            messages.add(message)
            schedule()
        }

        private fun schedule() {
            if (scheduled.compareAndSet(false, true)) executor.execute(::drain)
        }

        // a message whose handling fails is skipped, as an actor resumed on a failure does
        private fun drain() {
            try {
                while (true) {
                    val message = messages.poll() ?: break
                    runCatching { node.receive(if (message is String) MessageCodec.decode(message) else message as KbMessage) }
                        .onFailure { it.printStackTrace() }
                }
            } finally {
                scheduled.set(false)
                // a message posted after the last poll, before scheduled was reset
                if (messages.isNotEmpty()) schedule()
            }
        }
    }
}
