package it.unibo.tuprolog.argumentation.parallel.consumer

import it.unibo.tuprolog.argumentation.parallel.EvaluationStats
import it.unibo.tuprolog.argumentation.parallel.StatsCodec
import it.unibo.tuprolog.argumentation.parallel.host.HostCodec
import it.unibo.tuprolog.argumentation.parallel.host.HostFrame
import it.unibo.tuprolog.argumentation.parallel.host.Mailbox
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter
import it.unibo.tuprolog.argumentation.parallel.store.MessageConnection
import it.unibo.tuprolog.argumentation.parallel.store.SharedGraph

/**
 * What the solver of a node (see [ClientSolver]) has of its node: its clients, and the graphs it builds with other
 * consumers. The node itself when the two share a thread's reach ([it.unibo.tuprolog.argumentation.parallel.ParallelNode]),
 * frames to it otherwise ([ThreadEnvironment]).
 */
internal interface SolverEnvironment {
    /** [message] to the client [to] */
    fun reply(
        to: String,
        message: KbMessage,
    )

    /** The consumers a graph may have, wherever they run: null for no limit */
    val capacity: Int?

    /**
     * A new graph of [count] consumers, explored for a query if [exploring], the graph before given up: the solver its
     * consumer 0, the others those of [starts], by consumer. Those the node starts on its hosts are running when this
     * returns.
     */
    fun newGraph(
        count: Int,
        exploring: Boolean,
        starts: Map<Int, ConsumerStart>,
    ): NewGraph

    /** How to run a consumer on a thread of its own beside the solver, reaching the store directly */
    fun apart(): ((() -> Unit) -> Unit)? = null

    /** The solver keeps none of the graphs it made: the node need not keep them either */
    fun forgetGraphs()

    /** The arguments and the attacks each consumer of [graph] published, by consumer */
    fun published(graph: SharedGraph): Map<Int, Pair<Int, Int>>

    /** The solver answered an evaluation, as [stats] tell */
    fun evaluated(stats: EvaluationStats)
}

/** A graph just made, and the consumers left for the solver to run itself (see [SolverEnvironment.apart]) */
internal class NewGraph(
    val graph: SharedGraph,
    val here: Map<Int, ConsumerStart>,
)

/** What the solver of a node asks its node from a thread of its own (see [ThreadEnvironment]) */
internal object Requests {
    const val CAPACITY = "capacity"
    const val GRAPH = "graph"
    const val PUBLISHED = "published"
    const val FORGOTTEN = "forgotten"
    const val EVALUATED = "evaluated"

    /** A new graph (see [SolverEnvironment.newGraph]), as text */
    fun encodeGraph(
        count: Int,
        exploring: Boolean,
        starts: Map<Int, ConsumerStart>,
    ): String =
        TextWriter()
            .number(count)
            .text(if (exploring) "1" else "0")
            .list(starts.entries.toList()) { text(HostCodec.encode(HostFrame.Start("", it.key, it.value))) }
            .toString()

    /** The graph of [text] (see [encodeGraph]) */
    fun decodeGraph(text: String): Triple<Int, Boolean, Map<Int, ConsumerStart>> =
        TextReader(text).run {
            val count = number()
            val exploring = text() == "1"
            val starts = list { HostCodec.decode(text()) as HostFrame.Start }
            Triple(count, exploring, starts.associate { it.consumer to it.start })
        }

    /** The session of a new graph, and the host running each of its consumers */
    fun encodePlaced(
        session: String,
        sites: List<String>,
    ): String = TextWriter().text(session).texts(sites).toString()

    fun decodePlaced(text: String): Pair<String, List<String>> = TextReader(text).run { text() to texts() }

    fun encode(published: Map<Int, Pair<Int, Int>>): String =
        TextWriter().list(published.entries.toList()) { number(it.key).number(it.value.first).number(it.value.second) }.toString()

    fun decodePublished(text: String): Map<Int, Pair<Int, Int>> =
        TextReader(text).run { list { number() to (number() to number()) }.toMap() }
}

/**
 * The environment of a solver on a thread of its own, its node on another: what it asks sent by [send], its reply waited
 * for in [mailbox]. The node keeps the stores and starts the consumers: the solver is the consumer 0 of each graph,
 * reaching its store through frames as any other.
 */
internal class ThreadEnvironment(
    private val send: (HostFrame) -> Unit,
    private val mailbox: Mailbox,
) : SolverEnvironment {
    private var calls = 0

    // the session of each graph the solver keeps, for what it asks of it later
    private val sessions = HashMap<SharedGraph, String>()

    private fun ask(
        kind: String,
        payload: String = "",
    ): String {
        val number = calls++
        send(HostFrame.Ask(number, kind, payload))
        return (mailbox.await { it is HostFrame.Reply && it.call == number } as HostFrame.Reply).payload
    }

    override fun reply(
        to: String,
        message: KbMessage,
    ) = send(HostFrame.Response(to, MessageCodec.encode(message)))

    override val capacity: Int? get() = ask(Requests.CAPACITY).toIntOrNull()

    override fun newGraph(
        count: Int,
        exploring: Boolean,
        starts: Map<Int, ConsumerStart>,
    ): NewGraph {
        val (session, sites) = Requests.decodePlaced(ask(Requests.GRAPH, Requests.encodeGraph(count, exploring, starts)))
        val connection = MessageConnection(ThreadLine(session, 0, send, mailbox))
        val graph = SharedGraph.remote(exploring, connection)
        graph.sites = sites
        sessions[graph] = session
        return NewGraph(graph, emptyMap())
    }

    override fun published(graph: SharedGraph): Map<Int, Pair<Int, Int>> =
        sessions[graph]?.let { Requests.decodePublished(ask(Requests.PUBLISHED, it)) }.orEmpty()

    override fun forgetGraphs() {
        sessions.clear()
        send(HostFrame.Ask(-1, Requests.FORGOTTEN, ""))
    }

    override fun evaluated(stats: EvaluationStats) = send(HostFrame.Ask(-1, Requests.EVALUATED, StatsCodec.encode(stats)))
}
