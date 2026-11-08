@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.host.thisModule
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import kotlin.js.JsExport
import kotlin.js.Promise

/**
 * A parallel solver of JavaScript: nothing of it on the calling thread but this object, its node on a thread of its own, which never
 * waits, its solver and the consumers it runs on other threads, each waiting on a SharedArrayBuffer for what its node writes
 * it.
 */
@JsExport
abstract class ThreadSolver internal constructor(
    /** The node of the parallel solver, for what stands on it (see the module federation) */
    @JsExport.Ignore val node: NodeProxy,
) : ParallelSolver(node.access) {
    // The replies of the node: rejected if pending when the parallel solver closes
    private val replies = ClosableReplies()

    init {
        node.onFailed = { reason -> replies.refuse(reason) }
    }

    /** The identifier of the parallel solver */
    val id: String get() = node.id

    /** The statements answering [statement], as Prolog text, by label, each with the arguments for it (rejected if the parallel solver closes meanwhile) */
    fun solve(statement: String): Promise<EvaluationResponse> =
        replies.replied<EvaluationResponse> { access.solve(statement, it) }.then { answered(it) }

    /** The parallel solver as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries */
    val goals: AsyncGoalSolver = AsyncGoalSolver(access, replies)

    /** Stops the threads, and what stands on the parallel solver: the evaluations pending are rejected */
    fun close(): Promise<Unit> {
        replies.close()
        return node.close()
    }
}

/**
 * A distributed solver on the threads of Node.js (worker_threads), [threads] consumers on as many threads, one more for its
 * node, each loading this module ([module], this very file by default). The theory is split into at most [parts] partitions
 * if given, as many as the consumers it can run otherwise; the graph is labelled as the standard solver does with [flags].
 * The threads keep Node.js running only while an evaluation is pending; [close] stops them.
 */
@JsExport
class WorkerSolver(
    threads: Int,
    parts: Int? = null,
    flags: Flags = Flags.defaults(),
    module: String? = null,
) : ThreadSolver(NodeProxy.spawn(threads, parts, flags, module ?: thisModule()))

/** The replies of a node: those pending rejected when the parallel solver closes, and those asked for after */
internal class ClosableReplies : AsyncReplies {
    private val pending = mutableSetOf<(Throwable) -> Unit>()
    private var closed = false
    private var refusal = "The solver is closed"

    override fun <T> replied(ask: ((T) -> Unit) -> Unit): Promise<T> =
        Promise { resolve, reject ->
            if (closed) return@Promise reject(Arg2pException(refusal))
            pending += reject
            ask {
                if (pending.remove(reject)) resolve(it)
            }
        }

    fun close() = refuse("The solver is closed")

    /** A thread of the solver died, for [reason]: nothing can be answered any more */
    fun refuse(reason: String) {
        closed = true
        refusal = reason
        pending.toList().also { pending.clear() }.forEach { it(Arg2pException(reason)) }
    }
}
