package it.unibo.tuprolog.argumentation.parallel.consumer

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.graph.GraphSharing
import it.unibo.tuprolog.argumentation.core.notation.RuleAnnotations
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.vm.SolverSession
import it.unibo.tuprolog.argumentation.parallel.host.HostCodec
import it.unibo.tuprolog.argumentation.parallel.host.HostFrame
import it.unibo.tuprolog.argumentation.parallel.host.Inbox
import it.unibo.tuprolog.argumentation.parallel.host.Mailbox
import it.unibo.tuprolog.argumentation.parallel.message.MessageCodec
import it.unibo.tuprolog.argumentation.parallel.store.MessageConnection
import it.unibo.tuprolog.argumentation.parallel.store.SharedGraph
import it.unibo.tuprolog.argumentation.parallel.store.StoreLine
import it.unibo.tuprolog.argumentation.parallel.store.StoreProtocol
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver

/**
 * What a consumer of a graph runs, wherever it runs: a solver of [theory] with [flags] solving [goal] (Prolog text),
 * building on the conclusions of the others of [interest] alone (all of them if null), the graph explored for a query if
 * [exploring]
 */
@InternalArg2pApi
data class ConsumerStart(
    val theory: String,
    val flags: String,
    val goal: String,
    val interest: Set<String>?,
    val exploring: Boolean,
) {
    /** The solver of the consumer, made and joined to its graph by [join], solving the goal: why it failed, if it did */
    fun run(join: (Solver) -> Unit): String? =
        try {
            val worker = Arg2pSolverFactory.solverOf(Arg2pSolver.default(listOf(Flags.parse(flags).library())), "")
            worker.loadStaticKb(RuleAnnotations.parse(theory, worker.operators, worker.unificator))
            join(worker)
            val solution = worker.solveOnce(Struct.parse(goal, worker.operators))
            if (solution.isYes) null else solution.toString()
        } catch (failure: Throwable) {
            failure.message ?: failure.toString()
        }
}

/**
 * The line of the consumer [consumer] of the graph of [session] run on a thread: each call a frame sent by [send], its
 * answer waited for in [mailbox], or that the graph was given up
 */
internal class ThreadLine(
    private val session: String,
    private val consumer: Int,
    private val send: (HostFrame) -> Unit,
    private val mailbox: Mailbox,
) : StoreLine {
    private var calls = 0

    override fun tell(
        kind: String,
        payload: String,
    ) = send(HostFrame.Call(session, consumer, -1, kind, payload))

    override fun call(
        kind: String,
        payload: String,
    ): String {
        val number = calls++
        send(HostFrame.Call(session, consumer, number, kind, payload))
        val answer =
            mailbox.await {
                (it is HostFrame.Answer && it.session == session && it.consumer == consumer && it.call == number) ||
                    (it is HostFrame.Abandon && it.session == session && it.consumer == consumer)
            }
        return if (answer is HostFrame.Answer) answer.delivery else StoreProtocol.failure((answer as HostFrame.Abandon).reason)
    }
}

/**
 * The loop of a thread running consumers, one at a time, until it is told to stop: each read from [inbox], solved by a
 * solver of its own reaching its store through frames sent by [send]
 */
@InternalArg2pApi
fun runConsumerThread(
    inbox: Inbox,
    send: (String) -> Unit,
) {
    val mailbox = Mailbox(inbox)
    val frames = { frame: HostFrame -> send(HostCodec.encode(frame)) }
    while (true) {
        val frame = mailbox.next()
        if (frame == HostFrame.Stop) return
        if (frame !is HostFrame.Start) continue
        val start = frame.start
        val connection = MessageConnection(ThreadLine(frame.session, frame.consumer, frames, mailbox))
        val graph = SharedGraph.remote(start.exploring)
        start
            .run { worker ->
                SolverSession.join(worker, SolverSession().also { it[GraphSharing.KEY] = graph })
                graph.join(worker, connection, start.interest)
            }?.let { connection.fail("the solver of part ${frame.consumer}: $it") }
        frames(HostFrame.Done)
    }
}

/**
 * The loop of the thread of the solver of a node, apart from its node: the messages of the clients read from [inbox] and
 * handled by a solver of [flags] (see [ClientSolver], at most [parts] partitions), what it sends going by [send]
 */
@InternalArg2pApi
fun runSolverThread(
    inbox: Inbox,
    send: (String) -> Unit,
    parts: Int?,
    flags: Flags,
) {
    val mailbox = Mailbox(inbox)
    val frames = { frame: HostFrame -> send(HostCodec.encode(frame)) }
    val solver = ClientSolver(ThreadEnvironment(frames, mailbox), parts, flags)
    while (true) {
        val frame = mailbox.next()
        if (frame == HostFrame.Stop) return
        if (frame is HostFrame.Client) solver.receive(MessageCodec.decode(frame.message))
    }
}
