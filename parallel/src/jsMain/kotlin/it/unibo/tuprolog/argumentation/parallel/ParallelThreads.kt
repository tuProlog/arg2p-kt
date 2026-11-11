@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.consumer.runConsumerThread
import it.unibo.tuprolog.argumentation.parallel.consumer.runSolverThread
import it.unibo.tuprolog.argumentation.parallel.host.ConsumerThreads
import it.unibo.tuprolog.argumentation.parallel.host.DRAINED
import it.unibo.tuprolog.argumentation.parallel.host.SequentialScheduler
import it.unibo.tuprolog.argumentation.parallel.host.SharedInbox
import it.unibo.tuprolog.argumentation.parallel.host.SharedOutbox
import it.unibo.tuprolog.argumentation.parallel.host.handled
import it.unibo.tuprolog.argumentation.parallel.host.newChannel
import it.unibo.tuprolog.argumentation.parallel.host.nodeModule
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter
import kotlin.js.Promise

/*
 * The threads of a parallel solver of JavaScript: its node on a worker of its own, which never waits (the state of the graphs it
 * builds, and what stands on it, see NodeService); its solver and the consumers it runs each on a worker that waits (see
 * SharedInbox); the parallel solver itself, on the thread that made it, reaching its node through messages (see NodeProxy).
 */

// ---- the messages between a parallel solver and its node, as text

private sealed interface Console {
    /** To the node: [method] with [args], answered by a [Reply] to [call] */
    data class Call(
        val call: Int,
        val method: String,
        val args: List<String>,
    ) : Console

    data class Reply(
        val call: Int,
        val failure: String?,
        val payload: String,
    ) : Console

    /** To the parallel solver: the last evaluation its node answered, sent before each reply */
    data class State(
        val evaluation: String?,
    ) : Console

    /** To the parallel solver: a thread of its node died, for [reason]: nothing can be answered any more */
    data class Failed(
        val reason: String,
    ) : Console

    /** Between what stands on the parallel solver and what stands on its node (see [NodeService]) */
    data class Service(
        val text: String,
    ) : Console

    /** To the parallel solver: nothing can stand on its node, for [reason] */
    data class ServiceRefused(
        val reason: String,
    ) : Console

    companion object {
        fun encode(message: Console): String =
            TextWriter()
                .apply {
                    when (message) {
                        is Call -> text("Call").number(message.call).text(message.method).texts(message.args)
                        is Reply -> text("Reply").number(message.call).texts(listOfNotNull(message.failure)).text(message.payload)
                        is State -> text("State").texts(listOfNotNull(message.evaluation))
                        is Failed -> text("Failed").text(message.reason)
                        is Service -> text("Service").text(message.text)
                        is ServiceRefused -> text("ServiceRefused").text(message.reason)
                    }
                }.toString()

        fun decode(text: String): Console =
            TextReader(text).run {
                when (val kind = text()) {
                    "Call" -> Call(number(), text(), texts())
                    "Reply" -> Reply(number(), texts().singleOrNull(), text())
                    "State" -> State(texts().singleOrNull())
                    "Failed" -> Failed(text())
                    "Service" -> Service(text())
                    "ServiceRefused" -> ServiceRefused(text())
                    else -> throw IllegalArgumentException("Unknown message $kind")
                }
            }
    }
}

// The methods a parallel solver calls on its node
private object Methods {
    const val FLAGS = "flags"
    const val LOAD = "load"
    const val ADD = "add"
    const val RESET = "reset"
    const val SOLVE = "solve"
    const val QUERY = "query"
    const val NEXT = "next"
    const val ANSWER = "answer"
    const val STOP = "stop"
    const val CLOSE = "close"
}

/**
 * What stands on a node, on its thread, which never waits: a federation, or any other way for its graphs to reach beyond
 * the thread: the text it exchanges with its counterpart on the thread of the parallel solver (see [NodeProxy.service]). A worker
 * is told how to make it when it starts (see [startWebWorker]).
 */
@InternalArg2pApi
interface NodeService {
    fun received(text: String)

    fun close()
}

// ---- the threads

private fun workerThreads(): dynamic = nodeModule("worker_threads")

/** Whether this runs on Node.js, rather than in a browser */
@InternalArg2pApi
fun onNode(): Boolean = js("typeof process !== 'undefined' && process.versions != null && process.versions.node != null") as Boolean

// Whether this thread can share memory with those it spawns: a browser allows it to pages isolated across origins alone
private fun canShareMemory(): Boolean =
    js("typeof SharedArrayBuffer !== 'undefined' && (typeof crossOriginIsolated === 'undefined' || crossOriginIsolated)") as Boolean

// Whether this thread is one of a parallel solver (in a browser: told by the first message it got)
private var solverThread = false

/** A thread of a parallel solver, as the thread that spawned it reaches it: a Worker of Node.js, or of a browser */
internal class SpawnedThread(
    private val worker: dynamic,
    private val node: Boolean,
) {
    fun post(message: String) {
        worker.postMessage(message)
    }

    fun onMessage(handler: (dynamic) -> Unit) {
        if (node) {
            worker.on("message", handler)
        } else {
            worker.onmessage = { event: dynamic -> handler(event.data) }
        }
    }

    fun onError(handler: (dynamic) -> Unit) {
        if (node) {
            worker.on("error", handler)
        } else {
            worker.onerror = { event: dynamic -> handler(event) }
        }
    }

    /** Whether the thread keeps its process running (a browser has none to keep) */
    fun keepAlive(on: Boolean) {
        if (!node) return
        if (on) worker.ref() else worker.unref()
    }

    fun terminate() {
        worker.terminate()
    }
}

/**
 * A thread of a parallel solver running [module], with [data] (its role, and what it needs): on Node.js the module itself, its
 * [startWorkerThread] called; in a browser the script at that address, calling [startWebWorker], [data] its first message
 */
internal fun spawnThread(
    module: String,
    data: dynamic,
): SpawnedThread {
    data.module = module
    val options: dynamic = js("({})")
    if (!onNode()) {
        options.type = "module"
        val constructor = js("Worker")
        val worker = js("new constructor(module, options)")
        worker.postMessage(data)
        return SpawnedThread(worker, node = false)
    }
    // (a thread loading a test module of Kotlin registers none of its tests: with no test framework, the module would run
    // them all as it loads, each parallel solver they make spawning more threads, that run them again)
    val code =
        "const { workerData } = require('worker_threads');\n" +
            "globalThis.kotlinTest ??= { adapterTransformer: () => ({ suite() {}, test() {} }) };\n" +
            "import(workerData.module).then(module => module.startWorkerThread(module.nodeService))" +
            ".catch(error => { throw new Error(error?.message ?? String(error)); });"
    options.eval = true
    options.workerData = data
    val thread = workerThreads().Worker
    return SpawnedThread(js("new thread(code, options)"), node = true)
}

// What a thread of a parallel solver has of the thread that spawned it: the port of a thread of Node.js, or the scope of a Web
// Worker
private class ParentThread(
    private val port: dynamic,
    private val node: Boolean,
) {
    fun post(message: String) {
        port.postMessage(message)
    }

    fun onMessage(handler: (dynamic) -> Unit) {
        if (node) {
            port.on("message", handler)
        } else {
            port.onmessage = { event: dynamic -> handler(event.data) }
        }
    }
}

// What a thread of a parallel solver is given: its role, its channel if it waits on one, the parts and the flags of the solver
private fun dataOf(
    role: String,
    channel: dynamic = null,
    parts: Int? = null,
    flags: Flags = Flags.defaults(),
): dynamic {
    val data: dynamic = js("({})")
    data.role = role
    data.channel = channel
    data.parts = parts ?: -1
    data.flags = flags.toString()
    return data
}

/**
 * The loop of a thread of a parallel solver of Node.js (see [WorkerSolver]): its node, its solver, or a thread of its consumers.
 * [service] makes what stands on the node, if anything may (see [NodeService]).
 */
@JsExport
fun startWorkerThread(service: dynamic = null) {
    startThread(workerThreads().workerData, ParentThread(workerThreads().parentPort, node = true), service)
}

/**
 * The loop of a Web Worker of a parallel solver in a browser (see [WebWorkerSolver]), for the script of its Web Workers to call,
 * with this module loaded: its node, its solver, or a thread of its consumers, as its first message tells. [service] makes
 * what stands on the node, if anything may (as nodeService, for a federation).
 */
@JsExport
fun startWebWorker(service: dynamic = null) {
    val scope = js("self")
    scope.onmessage = { event: dynamic ->
        scope.onmessage = null
        solverThread = true
        startThread(event.data, ParentThread(scope, node = false), service)
    }
}

// (the solver and the threads of the consumers never return until told: they wait on their channel for what the node writes them)
private fun startThread(
    data: dynamic,
    parent: ParentThread,
    service: dynamic,
) {
    val send = { frame: String -> parent.post(frame) }
    val drained = { parent.post(DRAINED) }
    when (data.role as String) {
        NODE -> startNode(data, parent, service)
        SOLVER -> {
            val parts = (data.parts as Int).takeIf { it > 0 }
            runSolverThread(SharedInbox(data.channel, drained), send, parts, Flags.parse(data.flags as String))
        }
        CONSUMER -> runConsumerThread(SharedInbox(data.channel, drained), send)
    }
}

private const val NODE = "node"
private const val SOLVER = "solver"
private const val CONSUMER = "consumer"

// Why the thread of [role] died, from what the error of a thread gives (what crosses threads may have lost its message)
private fun reasonOf(
    role: String,
    error: dynamic,
): String {
    val told = (error?.message as? String)?.takeIf { it.isNotEmpty() } ?: error?.toString() as? String
    return "The thread of the $role of the solver died: ${told ?: "for no reason told"}"
}

// The node of a parallel solver, on its thread: its solver and the threads of its consumers spawned from here, the messages of the
// parallel solver handled as they come. Where threads cannot share memory (a page of a browser not isolated across origins) none
// can wait for another: the node solves alone on this thread, and runs no consumer for others.
private fun startNode(
    data: dynamic,
    parent: ParentThread,
    service: dynamic,
) {
    val threads = data.threads as Int
    val flags = Flags.parse(data.flags as String)
    val parts = (data.parts as Int).takeIf { it > 0 }
    val module = data.module as String
    val id = data.id as String
    val spawned = mutableListOf<SpawnedThread>()
    lateinit var node: ParallelNode
    if (canShareMemory()) {
        val hosted = { role: String ->
            val channel = newChannel()
            val outbox = SharedOutbox(channel)
            val worker = spawnThread(module, dataOf(role, channel, parts, flags))
            worker.onMessage { frame: dynamic ->
                if (frame == DRAINED) outbox.flush() else handled { node.fromThread(outbox, frame as String) }
            }
            // (a thread that died answers nothing more: the parallel solver told, rather than left waiting for it)
            worker.onError { error: dynamic -> parent.post(Console.encode(Console.Failed(reasonOf(role, error)))) }
            spawned.add(worker)
            outbox
        }
        val solver = hosted(SOLVER)
        // (the node offers the threads of its consumers: one placed on it runs at once, none queued behind another)
        val offered = maxOf(1, threads - 1)
        val consumers = ConsumerThreads(id, offered, List(offered) { hosted(CONSUMER) }) { hosted(CONSUMER) }
        node = ParallelNode(SequentialScheduler(), offered, parts, flags, id = id, solverThread = solver, threads = consumers)
    } else {
        node = ParallelNode(SequentialScheduler(), 0, 1, flags, id = id)
    }
    val served = ServedNode(node, service, { parent.post(it) }) { spawned.forEach { it.terminate() } }
    parent.onMessage { text: dynamic -> handled { served.received(text as String) } }
}

/**
 * [node], on its thread, served to its parallel solver: the calls of the parallel solver answered by [send], its last evaluation sent
 * before each answer; [closing] stops the threads of the node
 */
private class ServedNode(
    private val node: ParallelNode,
    private val makeService: dynamic,
    private val send: (String) -> Unit,
    private val closing: () -> Unit,
) {
    private var service: NodeService? = null

    private fun reply(
        call: Int,
        payload: String = "",
        failure: String? = null,
    ) {
        send(Console.encode(Console.State(node.evaluation?.let { StatsCodec.encode(it) })))
        send(Console.encode(Console.Reply(call, failure, payload)))
    }

    fun received(text: String) {
        val message =
            when (val decoded = Console.decode(text)) {
                is Console.Call -> decoded
                is Console.Service -> return toService(decoded.text)
                else -> return
            }
        val (call, method, args) = message
        val answer = { response: KbMessage -> reply(call, MessageCodec.encode(response)) }
        when (method) {
            Methods.FLAGS -> reply(call).also { node.flags = Flags.parse(args[0]) }
            Methods.LOAD -> reply(call).also { node.load(args[0]) }
            Methods.ADD -> reply(call).also { node.add(args[0]) }
            Methods.RESET -> reply(call).also { node.reset() }
            Methods.SOLVE -> node.solve(args[0]) { answer(it) }
            Methods.QUERY -> node.query(args[0]) { answer(it) }
            Methods.NEXT -> node.next(args[0]) { answer(it) }
            Methods.ANSWER -> node.answer(args[0]) { answer(it) }
            Methods.STOP -> reply(call).also { node.stop() }
            Methods.CLOSE -> {
                service?.close()
                node.close()
                closing()
                reply(call)
            }
        }
    }

    private fun toService(text: String) {
        val running = service ?: make() ?: return
        running.received(text)
    }

    private fun make(): NodeService? {
        if (makeService == null || makeService == undefined) {
            send(
                Console.encode(
                    Console.ServiceRefused(
                        "The threads of this solver were not told how: the script of its workers calls startWebWorker(nodeService)",
                    ),
                ),
            )
            return null
        }
        val make = makeService
        val tell = { text: String -> send(Console.encode(Console.Service(text))) }
        val made: dynamic = make(node, tell)
        return (made as NodeService).also { service = it }
    }
}

/**
 * The node of a parallel solver on the thread [thread] (a worker), as the parallel solver reaches it: each call a message, its answer
 * given as it comes, its last evaluation as it was last told. The thread keeps Node.js running only while an answer is
 * waited for.
 */
@InternalArg2pApi
class NodeProxy internal constructor(
    private val thread: SpawnedThread,
    /** The identifier of the node */
    val id: String,
    flags: Flags,
) {
    private var calls = 0
    private val pending = HashMap<Int, Pair<(String) -> Unit, (String) -> Unit>>()
    private var lastEvaluation: EvaluationStats? = null
    private var holds = 0

    // why a thread of the node died, if one did
    private var death: String? = null

    private var fromService: (String) -> Unit = {}
    private var refused: (String) -> Unit = {}

    init {
        thread.keepAlive(false)
        thread.onMessage { text: dynamic -> handled { received(text as String) } }
        thread.onError { error: dynamic -> failed(reasonOf(NODE, error)) }
    }

    /** Called once a thread of the node died, with why: nothing can be answered any more */
    internal var onFailed: (String) -> Unit = {}

    /** Keeps Node.js running, until as many [release]s: what stands on the parallel solver waits for an answer */
    fun hold() {
        if (holds++ == 0) thread.keepAlive(true)
    }

    // (no lower than none: a reply that comes after a failure released them all)
    fun release() {
        if (holds > 0 && --holds == 0) thread.keepAlive(false)
    }

    /**
     * What stands on the parallel solver, reaching its counterpart on the thread of the node: what it says given to [received], and
     * why it cannot be there to [refusal]; how to tell it something returned
     */
    fun service(
        received: (String) -> Unit,
        refusal: (String) -> Unit,
    ): (String) -> Unit {
        fromService = received
        refused = refusal
        return { text -> thread.post(Console.encode(Console.Service(text))) }
    }

    // A thread died: the calls waiting fail, the thread of the node no longer keeping Node.js running
    private fun failed(reason: String) {
        onFailed(reason)
        shut(reason)
    }

    // Nothing answered any more, for [reason]: the calls waiting fail, what stands on the node is refused
    private fun shut(reason: String) {
        death = reason
        val waiting = pending.values.toList()
        pending.clear()
        holds = 0
        thread.keepAlive(false)
        refused(reason)
        waiting.forEach { (_, failed) -> failed(reason) }
    }

    private fun received(text: String) {
        when (val message = Console.decode(text)) {
            is Console.State -> lastEvaluation = message.evaluation?.let { StatsCodec.decodeEvaluation(it) }
            is Console.Reply -> {
                val (done, failed) = pending.remove(message.call) ?: return
                release()
                if (message.failure == null) done(message.payload) else failed(message.failure)
            }
            is Console.Failed -> failed(message.reason)
            is Console.Service -> fromService(message.text)
            is Console.ServiceRefused -> refused(message.reason)
            is Console.Call -> Unit
        }
    }

    private fun call(
        method: String,
        args: List<String>,
        done: (String) -> Unit,
        failed: (String) -> Unit,
    ) {
        death?.let { return failed(it) }
        val number = calls++
        hold()
        pending[number] = done to failed
        thread.post(Console.encode(Console.Call(number, method, args)))
    }

    // (a call whose answer nothing waits for)
    private fun tell(
        method: String,
        vararg args: String,
    ) = call(method, args.toList(), {}, { console.error(it) })

    private fun <T : KbMessage> answered(
        method: String,
        arg: String,
        reply: (T) -> Unit,
    ) = call(method, listOf(arg), {
        @Suppress("UNCHECKED_CAST")
        reply(MessageCodec.decode(it) as T)
    }) { console.error(it) }

    /** The parallel solver as it reaches its node */
    internal val access: NodeAccess =
        object : NodeAccess {
            override var flags: Flags = flags
                set(value) {
                    field = value
                    tell(Methods.FLAGS, value.toString())
                }

            override fun load(theory: String) = tell(Methods.LOAD, theory)

            override fun add(clause: String) = tell(Methods.ADD, clause)

            override fun reset() = tell(Methods.RESET)

            override val evaluation: EvaluationStats? get() = lastEvaluation

            override fun solve(
                statement: String,
                reply: (EvaluationResponse) -> Unit,
            ) = answered(Methods.SOLVE, statement, reply)

            override fun query(
                goal: String,
                reply: (SolutionResponse) -> Unit,
            ) = answered(Methods.QUERY, goal, reply)

            override fun next(
                run: String,
                reply: (SolutionResponse) -> Unit,
            ) = answered(Methods.NEXT, run, reply)

            override fun answer(
                query: String,
                reply: (AnswerResponse) -> Unit,
            ) = answered(Methods.ANSWER, query, reply)

            override fun stop() = tell(Methods.STOP)
        }

    // (closed all the same if the thread of the node died)
    internal fun close(): Promise<Unit> {
        // (what stands on the node, a federation say, refused from now on: its calls answered, not left waiting)
        val stop: (Any?) -> Unit = {
            thread.terminate()
            shut("The solver is closed")
        }
        return Promise<String> { resolve, reject ->
            call(Methods.CLOSE, emptyList(), { resolve(it) }, { reject(Arg2pException(it)) })
        }.then(stop, stop)
    }

    internal companion object {
        /** The node of a parallel solver of [threads] threads on a thread of its own, running [module] */
        fun spawn(
            threads: Int,
            parts: Int?,
            flags: Flags,
            module: String,
        ): NodeProxy {
            // (a thread of a parallel solver makes no parallel solver: nothing that runs on one can spawn threads without end)
            val inSolver = if (onNode()) workerThreads().workerData?.role != null else solverThread
            if (inSolver == true) throw Arg2pException("A thread of a solver cannot make a solver")
            val id = ParallelNode.newId()
            val data = dataOf(NODE, parts = parts, flags = flags)
            data.threads = maxOf(2, threads)
            data.id = id
            return NodeProxy(spawnThread(module, data), id, flags)
        }
    }
}
