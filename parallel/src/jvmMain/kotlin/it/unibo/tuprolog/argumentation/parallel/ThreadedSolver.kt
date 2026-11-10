package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.consumer.runConsumerThread
import it.unibo.tuprolog.argumentation.parallel.consumer.runSolverThread
import it.unibo.tuprolog.argumentation.parallel.host.ConsumerThreads
import it.unibo.tuprolog.argumentation.parallel.host.PoolScheduler
import it.unibo.tuprolog.argumentation.parallel.host.TaskQueue
import it.unibo.tuprolog.argumentation.parallel.host.ThreadInbox
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException
import java.util.concurrent.ConcurrentHashMap

/**
 * A distributed solver on the threads of this JVM: the solver of its theory on one of `threads` threads, the other consumers
 * of the graphs it builds on threads of their own (see [it.unibo.tuprolog.argumentation.parallel.store.SharedGraph]), at most
 * `threads` for a graph. The theory is split into at most `parts` partitions if given, as many as the consumers it can run
 * otherwise; the graph is labelled as the standard solver does with `flags`. With `textual`, the messages of the clients to
 * the solver are written as text and read again (see [it.unibo.tuprolog.argumentation.parallel.message.MessageCodec]).
 */
class ThreadedSolver private constructor(
    private val scheduler: PoolScheduler,
    /** The node of the parallel solver, for what stands on it (see the module federation) */
    @property:InternalArg2pApi val node: ParallelNode,
) : ParallelSolver(LocalAccess(node)),
    AutoCloseable {
    @JvmOverloads
    constructor(
        threads: Int = processors,
        parts: Int? = null,
        flags: Flags = Flags.defaults(),
        textual: Boolean = false,
    ) : this(PoolScheduler(threads), threads, parts, flags, textual)

    private constructor(
        scheduler: PoolScheduler,
        threads: Int,
        parts: Int?,
        flags: Flags,
        textual: Boolean,
    ) : this(scheduler, node(scheduler, threads, parts, flags, textual))

    // The replies of the node, waited for: the evaluations of several callers run in parallel, those waiting when the
    // parallel solver closes failing then
    private val replies = Waited()

    /** The identifier of the parallel solver */
    val id: String get() = node.id

    /**
     * The statements answering [statement], as Prolog text, by label, each with the arguments for it: the evaluations
     * of several callers run in parallel (an Arg2pException if the parallel solver closes meanwhile)
     */
    fun solve(statement: String): EvaluationResponse = answered(replies.replied { node.solve(statement, it) })

    /**
     * The parallel solver as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries. The goals
     * of several callers are solved in turn by the client solver, each solved again from its start when another one was
     * solved since its last solution.
     */
    val goals: GoalSolver = GoalSolver(access, replies)

    /** Closes what stands on the parallel solver, and stops the threads: the evaluations waiting fail */
    override fun close() {
        replies.close()
        node.close()
        scheduler.close()
    }

    // The replies of the node, each waited for by the thread asking: those waiting when the parallel solver closes, and those
    // asked for since, an Arg2pException
    private class Waited : Replies {
        private val waiting = ConcurrentHashMap.newKeySet<CompletableFuture<*>>()

        @Volatile private var closed = false

        override fun <T> replied(ask: ((T) -> Unit) -> Unit): T {
            val response = CompletableFuture<T>()
            waiting += response
            try {
                if (closed) throw CLOSED
                ask { response.complete(it) }
                return response.join()
            } catch (failure: CompletionException) {
                throw failure.cause ?: failure
            } catch (failure: java.util.concurrent.RejectedExecutionException) {
                // (the threads stopped while asking: the parallel solver closed)
                throw if (closed) CLOSED else failure
            } finally {
                waiting -= response
            }
        }

        fun close() {
            closed = true
            waiting.forEach { it.completeExceptionally(CLOSED) }
        }
    }

    companion object {
        // A node on [scheduler], the consumers it runs for others each on a thread that waits, made as they are needed
        private fun node(
            scheduler: PoolScheduler,
            threads: Int,
            parts: Int?,
            flags: Flags,
            textual: Boolean,
        ): ParallelNode {
            lateinit var node: ParallelNode
            val id = ParallelNode.newId()
            val consumers = ConsumerThreads(id, threads) { consumerThread({ node }, null) }
            node = ParallelNode(scheduler, threads, parts, flags, textual, id, threads = consumers)
            return node
        }

        // A thread running consumers, its frames going to its node: through [queue] if given, on the thread itself otherwise
        private fun consumerThread(
            node: () -> ParallelNode,
            queue: TaskQueue?,
        ): ThreadInbox {
            val inbox = ThreadInbox()
            val deliver = toNode(inbox, node, queue)
            Thread({ runConsumerThread(inbox, deliver) }, "arg2p-consumer").apply { isDaemon = true }.also { inbox.thread = it }.start()
            return inbox
        }

        private fun toNode(
            inbox: ThreadInbox,
            node: () -> ParallelNode,
            queue: TaskQueue?,
        ): (String) -> Unit =
            if (queue == null) {
                { frame -> node().fromThread(inbox, frame) }
            } else {
                { frame -> queue.post { node().fromThread(inbox, frame) } }
            }

        /**
         * A parallel solver laid out as one of JavaScript is (see WorkerSolver): its node on its threads, never waiting, its solver
         * and the consumers it runs each on a thread of its own that may wait, the two talking through frames as text alone
         * (see [it.unibo.tuprolog.argumentation.parallel.host.HostFrame]), [threads] consumers in all
         */
        @InternalArg2pApi
        fun detached(
            threads: Int,
            parts: Int? = null,
            flags: Flags = Flags.defaults(),
        ): ThreadedSolver {
            val scheduler = PoolScheduler(threads)
            val queue = scheduler.queue()
            val solver = ThreadInbox()
            val offered = maxOf(1, threads - 1)
            val id = ParallelNode.newId()
            lateinit var node: ParallelNode
            val make = { consumerThread({ node }, queue) }
            // (the node offers the threads of its consumers: one placed on it runs at once, none queued behind another)
            val consumers = ConsumerThreads(id, offered, List(offered) { make() }, make)
            node = ParallelNode(scheduler, offered, parts, flags, id = id, solverThread = solver, threads = consumers)
            val deliver = toNode(solver, { node }, queue)
            Thread(
                { runSolverThread(solver, deliver, parts, flags) },
                "arg2p-solver",
            ).apply { isDaemon = true }.also { solver.thread = it }.start()
            return ThreadedSolver(scheduler, node)
        }

        /** The threads of a parallel solver by default: as many as the processors */
        @JvmStatic
        val processors: Int get() = Runtime.getRuntime().availableProcessors()

        private val CLOSED get() = Arg2pException("The solver is closed")
    }
}
