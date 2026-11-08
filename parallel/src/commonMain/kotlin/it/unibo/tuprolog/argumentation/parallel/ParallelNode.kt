package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.consumer.ClientSolver
import it.unibo.tuprolog.argumentation.parallel.consumer.ConsumerStart
import it.unibo.tuprolog.argumentation.parallel.consumer.NewGraph
import it.unibo.tuprolog.argumentation.parallel.consumer.Requests
import it.unibo.tuprolog.argumentation.parallel.consumer.SolverEnvironment
import it.unibo.tuprolog.argumentation.parallel.host.ConsumerHost
import it.unibo.tuprolog.argumentation.parallel.host.ConsumerThreads
import it.unibo.tuprolog.argumentation.parallel.host.HostCodec
import it.unibo.tuprolog.argumentation.parallel.host.HostFrame
import it.unibo.tuprolog.argumentation.parallel.host.HostedThread
import it.unibo.tuprolog.argumentation.parallel.host.Scheduler
import it.unibo.tuprolog.argumentation.parallel.host.TaskQueue
import it.unibo.tuprolog.argumentation.parallel.host.locked
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.Configure
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.message.NO_GRAPH
import it.unibo.tuprolog.argumentation.parallel.message.Rebuild
import it.unibo.tuprolog.argumentation.parallel.message.RequireAnswer
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.RequireSolution
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.message.STOPPED
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.parallel.message.Stop
import it.unibo.tuprolog.argumentation.parallel.store.SharedGraph
import it.unibo.tuprolog.argumentation.parallel.store.SharedStore
import it.unibo.tuprolog.argumentation.parallel.store.StoreProtocol
import kotlin.concurrent.Volatile
import kotlin.random.Random

/**
 * The part of a parallel solver that never waits: the clients and their solver ([ClientSolver], on [solverThread] if it is run apart
 * from here, a queue of [scheduler] otherwise), the stores of the graphs the solver builds, and the hosts the consumers of
 * those graphs run on. The consumers are placed on this node and on the hosts given ([addHost]) in proportion to what each
 * offers; those of other nodes run here on [threads], which offer [workers] consumers (null for no limit).
 *
 * It knows nothing of what is beyond its hosts: a host may be a federated site, or anything else that takes a consumer and
 * carries [HostFrame]s.
 *
 * A graph built is a session of the node: a new one gives up the consumers still running of the one before. If a host
 * running consumers of the last graph leaves, the graph fails and is built again with the hosts left.
 */
@InternalArg2pApi
class ParallelNode(
    private val scheduler: Scheduler,
    /** The consumers of a graph this node offers: null for no limit */
    val workers: Int?,
    private val parts: Int? = null,
    flags: Flags = Flags.defaults(),
    private val textual: Boolean = false,
    val id: String = newId(),
    private val solverThread: HostedThread? = null,
    /** The threads running the consumers this node runs for others (its own too, with [solverThread]) */
    val threads: ConsumerThreads? = null,
) {
    private val lock = Any()

    @Volatile private var hosts: List<ConsumerHost> = emptyList()

    // The stores of the graphs the solver keeps (it reads them again for later goals), the last ones, by session; the
    // host of each consumer of each, for the answers of its store; and the session built last
    @Volatile private var stores: Map<String, SharedStore> = emptyMap()

    @Volatile private var placed: Map<String, Map<Int, ConsumerHost>> = emptyMap()

    @Volatile private var session: String = "$id#0"

    private var sessions = 0

    /** The last evaluation the solver answered */
    @Volatile var evaluation: EvaluationStats? = null
        private set

    // The clients waiting for an answer: how each is given it, and what it is given if stopped
    private val clients = mutableMapOf<String, Pair<(KbMessage) -> Unit, KbMessage>>()
    private var clientIds = 0
    private var runIds = 0

    private val solverQueue: TaskQueue = scheduler.queue()

    // Whether the solver runs apart from this node: its consumers then all run on [threads], the store reached by frames
    private val detached: Boolean get() = solverThread != null

    // (made at its first message: a node may only run consumers for others)
    private val clientSolver: ClientSolver? by lazy {
        if (solverThread == null) ClientSolver(Environment(), parts, flags) else null
    }

    // [message] handled by the solver: here, on the queue of the solver, or written to its thread
    private fun solver(message: KbMessage) {
        val here = clientSolver
        if (here != null) {
            here.receive(message)
        } else {
            solverThread!!.put(HostCodec.encode(HostFrame.Client(MessageCodec.encode(message))))
        }
    }

    // The solver as a host of the consumer 0 of its graphs: a thread reached by frames
    private val solverHost: ConsumerHost? =
        solverThread?.let { thread ->
            object : ConsumerHost {
                override val id: String get() = this@ParallelNode.id
                override val capacity: Int get() = 1

                override fun start(
                    start: HostFrame.Start,
                    toStore: (HostFrame.Call) -> Unit,
                ) = Unit

                override fun answer(answer: HostFrame.Answer) = thread.put(HostCodec.encode(answer))

                override fun abandon(abandon: HostFrame.Abandon) = Unit
            }
        }

    /** A place for the consumers of the next graphs */
    fun addHost(host: ConsumerHost) = locked(lock) { hosts = hosts + host }

    /** [host] left: if it runs consumers of the last graph, the graph fails, and is built again by the next goal needing it */
    fun removeHost(host: ConsumerHost) {
        val running =
            locked(lock) {
                hosts = hosts - host
                session.takeIf { current -> placed[current]?.values?.any { it === host } == true }
            } ?: return
        stores[running]?.fail("a host building the graph left", lost = true)
        deliver(Rebuild) { running == session }
    }

    /** What a consumer asks of its store, from whatever host: its answer goes back through the host */
    fun toStore(call: HostFrame.Call) {
        val store = stores[call.session]
        when {
            // (served on the thread of the node, or of a link: the store never waits)
            store != null -> StoreProtocol.serve(store, call.consumer, call.kind, call.payload) { answer(call, it) }
            // a graph given up here: the consumer waiting is told, rather than left waiting
            call.call >= 0 -> answer(call, StoreProtocol.failure("the graph of ${call.session} is no longer built"))
        }
    }

    private fun answer(
        call: HostFrame.Call,
        delivery: String,
    ) {
        placed[call.session]?.get(call.consumer)?.answer(HostFrame.Answer(call.session, call.consumer, call.call, delivery))
    }

    /**
     * [frame], from [from], a thread of this node, on a thread that never waits: what the solver asks, its answers to its
     * clients, what the consumers ask of their stores
     */
    fun fromThread(
        from: HostedThread,
        frame: String,
    ) {
        when (val decoded = HostCodec.decode(frame)) {
            is HostFrame.Response -> locked(lock) { clients.remove(decoded.client) }?.first?.invoke(MessageCodec.decode(decoded.message))
            is HostFrame.Ask -> {
                val reply = asked(decoded.kind, decoded.payload)
                if (decoded.call >= 0) from.put(HostCodec.encode(HostFrame.Reply(decoded.call, reply)))
            }
            is HostFrame.Call ->
                if (from === solverThread) {
                    toStore(decoded)
                } else {
                    threads?.received(from, decoded)
                }
            else -> threads?.received(from, decoded)
        }
    }

    // What the solver on its thread asks, answered
    private fun asked(
        kind: String,
        payload: String,
    ): String =
        when (kind) {
            Requests.CAPACITY -> capacity?.toString() ?: ""
            Requests.GRAPH -> {
                val (count, _, starts) = Requests.decodeGraph(payload)
                val store = SharedStore(count).also { it.remote = true }
                val hosts = open(count, store)
                // the store kept here, every consumer reaching it through frames
                startOn(hosts, starts, session)
                Requests.encodePlaced(session, sitesOf(hosts, count))
            }
            Requests.PUBLISHED -> Requests.encode(stores[payload]?.published().orEmpty())
            Requests.FORGOTTEN -> "".also { forgetGraphs() }
            Requests.EVALUATED -> "".also { evaluation = StatsCodec.decodeEvaluation(payload) }
            else -> ""
        }

    // A new session for [store] of [count] consumers: their hosts by consumer, the consumer 0 here (none for those to
    // run by the solver), the consumers still running of the session before given up
    private fun open(
        count: Int,
        store: SharedStore,
    ): Map<Int, ConsumerHost?> {
        val (before, beforeHosts, chosen) =
            locked(lock) {
                val before = session
                val next = "$id#${++sessions}"
                val chosen = hostsFor(count)
                session = next
                stores = stores + (next to store)
                placed = placed + (next to chosen.filterValues { it != null }.mapValues { it.value!! })
                if (stores.size > GRAPHS_KEPT) {
                    val oldest = stores.keys.first()
                    stores = stores - oldest
                    placed = placed - oldest
                }
                Triple(before, placed[before].orEmpty(), chosen)
            }
        // (out of the lock: a host may answer on this thread)
        beforeHosts.forEach { (consumer, host) ->
            host.abandon(HostFrame.Abandon(before, consumer, "the graph of $id is no longer built"))
        }
        return chosen
    }

    // (under the lock) The hosts of the [count] consumers of a graph, in proportion to what each offers, each given
    // consecutive ones: the consumer 0 the solver, null for those running beside the solver
    private fun hostsFor(count: Int): Map<Int, ConsumerHost?> {
        val here: ConsumerHost? = if (detached) threads else null
        val offeredHere =
            if (detached) {
                threads?.capacity ?: 0
            } else if (scheduler.apart() == null) {
                0
            } else {
                workers?.let { maxOf(0, it - 1) }
            }
        val others = hosts
        val weights = (listOf(offeredHere) + others.map { it.capacity }).map { (it ?: (count - 1)).toDouble() }
        val total = weights.sum()
        val rest = (1 until count).toList()
        val shares = if (total == 0.0) listOf(rest.size) + others.map { 0 } else shares(weights, total, rest.size)
        val places = listOf(here) + others
        val assigned = places.indices.flatMap { place -> List(shares[place]) { places[place] } }
        return mapOf<Int, ConsumerHost?>(0 to solverHost) + rest.zip(assigned).toMap()
    }

    private fun shares(
        weights: List<Double>,
        total: Double,
        count: Int,
    ): List<Int> {
        val quotas = weights.map { it * count / total }
        val shares = quotas.map { it.toInt() }.toMutableList()
        quotas.indices
            .sortedByDescending { quotas[it] - shares[it] }
            .take(count - shares.sum())
            .forEach { shares[it]++ }
        return shares
    }

    // The consumers of [starts] started on [hosts]: those with one (those left, for the solver to run, are returned)
    private fun startOn(
        hosts: Map<Int, ConsumerHost?>,
        starts: Map<Int, ConsumerStart>,
        session: String,
    ): Map<Int, ConsumerStart> {
        val (placedOn, here) = starts.entries.partition { hosts[it.key] != null }
        placedOn.forEach { (consumer, start) ->
            hosts.getValue(consumer)!!.start(HostFrame.Start(session, consumer, start), ::toStore)
        }
        return here.associate { it.key to it.value }
    }

    private fun sitesOf(
        hosts: Map<Int, ConsumerHost?>,
        count: Int,
    ): List<String> = List(count) { hosts[it]?.id ?: id }

    // The solver keeps no graph any more: neither does this node
    private fun forgetGraphs() =
        locked(lock) {
            stores = emptyMap()
            placed = emptyMap()
        }

    // The consumers this node runs (its solver alone if it cannot run others) and those of the hosts, no limit if one has none
    private val capacity: Int?
        get() {
            val own = threads?.takeIf { detached }?.let { it.capacity + 1 } ?: if (scheduler.apart() == null) 1 else workers
            val lent = hosts.map { it.capacity }
            return if (own == null || null in lent) null else maxOf(1, own + lent.sumOf { it ?: 0 })
        }

    // The solver as it sees this node, on a queue of this process
    private inner class Environment : SolverEnvironment {
        override fun reply(
            to: String,
            message: KbMessage,
        ) {
            locked(lock) { clients.remove(to) }?.first?.invoke(message)
        }

        override val capacity: Int? get() = this@ParallelNode.capacity

        override fun newGraph(
            count: Int,
            exploring: Boolean,
            starts: Map<Int, ConsumerStart>,
        ): NewGraph {
            val graph = SharedGraph(count, exploring)
            val hosts = open(count, graph.state!!)
            graph.sites = sitesOf(hosts, count)
            // (those of other hosts known first: what those here publish then goes with its terms)
            graph.remoteConsumers(starts.keys.filter { hosts[it] != null })
            return NewGraph(graph, startOn(hosts, starts, session))
        }

        override fun apart(): ((() -> Unit) -> Unit)? = scheduler.apart()

        override fun forgetGraphs() = this@ParallelNode.forgetGraphs()

        override fun published(graph: SharedGraph): Map<Int, Pair<Int, Int>> = graph.published()

        override fun evaluated(stats: EvaluationStats) {
            evaluation = stats
        }
    }

    // ---- the clients

    /** The flags of the solver: those set apply from the next evaluation on, the graph built anew */
    var flags: Flags = flags
        set(value) {
            field = value
            deliver(Configure(value.toString()))
        }

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = deliver(Load(theory))

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = deliver(Add(clause))

    /** Forgets the theory */
    fun reset() = deliver(Reset)

    /** Evaluates [statement], as Prolog text: [reply] is given the arguments for it by label */
    fun solve(
        statement: String,
        reply: (EvaluationResponse) -> Unit,
    ) = ask({ RequireEvaluation(statement, it) }, EvaluationResponse(emptyList(), emptyList(), emptyList(), STOPPED)) {
        reply(it as EvaluationResponse)
    }

    // Solves [goal], Prolog text, as the standard solver does, in a run of its own: [reply] is given its first solution
    internal fun query(
        goal: String,
        reply: (SolutionResponse) -> Unit,
    ) {
        val run = locked(lock) { "run_${runIds++}" }
        ask({ RequireSolution(run, goal, it) }, stopped(run)) { reply(it as SolutionResponse) }
    }

    // The next solution of the goal solved in [run]
    internal fun next(
        run: String,
        reply: (SolutionResponse) -> Unit,
    ) = ask({ RequireSolution(run, null, it) }, stopped(run)) { reply(it as SolutionResponse) }

    // The statements for [query], Prolog text, by label, as the standard solver answers it
    internal fun answer(
        query: String,
        reply: (AnswerResponse) -> Unit,
    ) = ask({ RequireAnswer(query, it) }, AnswerResponse(null, STOPPED)) { reply(it as AnswerResponse) }

    /**
     * Stops what the solver evaluates for the clients: their evaluations end at once, as stopped, and the graph being built
     * for them is given up (the resolution under way interrupted, where it can be); the next ones are answered as before.
     * Nothing is done if none is under way.
     */
    fun stop() {
        val stopped = locked(lock) { clients.values.toList().also { clients.clear() } }
        if (stopped.isEmpty()) return
        solverQueue.interrupt()
        deliver(Stop)
        stopped.forEach { (reply, answer) -> reply(answer) }
    }

    // The arguments of the graph labelled last, for the tests
    internal val graphArguments: List<it.unibo.tuprolog.core.Term> get() = clientSolver?.graphArguments.orEmpty()

    private val closings = mutableListOf<() -> Unit>()

    /** Called as the node closes: what stands on it goes with it */
    fun onClose(closing: () -> Unit) = locked(lock) { closings += closing }

    /** What stands on the node closed, and its threads stopped */
    fun close() {
        val all = locked(lock) { closings.toList().also { closings.clear() } }
        all.forEach { it() }
        threads?.close()
        solverThread?.put(HostCodec.encode(HostFrame.Stop))
    }

    // The solver asked [request], for a client given its answer ([stopped] if stopped)
    private fun ask(
        request: (client: String) -> KbMessage,
        stopped: KbMessage,
        reply: (KbMessage) -> Unit,
    ) {
        val client = locked(lock) { "client_${clientIds++}".also { clients[it] = reply to stopped } }
        deliver(request(client))
    }

    // What a client solving a goal in [run] is given if stopped
    private fun stopped(run: String) = SolutionResponse(run, null, NO_GRAPH, "", stopped = true)

    // [message] for the solver, handled if [current] when it comes to it (what concerns a graph given up reaches none)
    private fun deliver(
        message: KbMessage,
        current: () -> Boolean = { true },
    ) {
        if (textual) {
            val text = MessageCodec.encode(message)
            solverQueue.post { if (current()) solver(MessageCodec.decode(text)) }
        } else {
            solverQueue.post { if (current()) solver(message) }
        }
    }

    companion object {
        // The graphs of its theory a node keeps: more than its solver does (the whole one, and one for each statement of
        // its last goals), which tells when it forgets them
        private const val GRAPHS_KEPT = 32

        // An identifier with no '#', which separates it from the number of a session
        fun newId() = "site-" + Random.nextLong().toULong().toString(16)
    }
}
