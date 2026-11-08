package it.unibo.tuprolog.argumentation.parallel.host

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.consumer.ConsumerStart
import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter
import it.unibo.tuprolog.argumentation.parallel.store.StoreProtocol

/**
 * A place where the consumers of a graph can run, as the node building the graph sees it: the threads of the node
 * ([ConsumerThreads]), another site of a federation, or any other medium that comes. Its consumers reach the store of
 * their graph through [HostFrame]s, as text, whatever carries them. No method waits.
 */
@InternalArg2pApi
interface ConsumerHost {
    /** Who it is, for the statistics */
    val id: String

    /** The consumers of a graph it offers: null for no limit */
    val capacity: Int?

    /** Runs the consumer [start] tells of: what it asks of its store goes to [toStore], on any thread */
    fun start(
        start: HostFrame.Start,
        toStore: (HostFrame.Call) -> Unit,
    )

    /** What the store answers a consumer run here */
    fun answer(answer: HostFrame.Answer)

    /** A consumer run here is given up: what it waits for fails, the graph no longer built */
    fun abandon(abandon: HostFrame.Abandon)
}

/** The frames between the consumers of a graph and its store, and between a node and the threads it runs them on */
@InternalArg2pApi
sealed interface HostFrame {
    /** To a host: runs the consumer [consumer] of the graph of [session] */
    data class Start(
        val session: String,
        val consumer: Int,
        val start: ConsumerStart,
    ) : HostFrame

    /** From a consumer to its store: its call [call] (-1 if answered by nothing, see StoreProtocol) */
    data class Call(
        val session: String,
        val consumer: Int,
        val call: Int,
        val kind: String,
        val payload: String,
    ) : HostFrame {
        companion object {
            /** What tells the store that the consumer [consumer] of the graph of [session] cannot go on, for [reason] */
            fun failing(
                session: String,
                consumer: Int,
                reason: String,
            ) = Call(session, consumer, -1, StoreProtocol.FAIL, reason)
        }
    }

    /** From a store to a consumer: the answer to its call [call], a delivery as text */
    data class Answer(
        val session: String,
        val consumer: Int,
        val call: Int,
        val delivery: String,
    ) : HostFrame

    /** To a consumer: the graph is given up */
    data class Abandon(
        val session: String,
        val consumer: Int,
        val reason: String,
    ) : HostFrame

    /** From a thread to its node: the consumer it ran ended, the thread is free */
    data object Done : HostFrame

    /** To a thread: its loop ends */
    data object Stop : HostFrame

    /** To the thread of the solver of a node: a message of its clients */
    data class Client(
        val message: String,
    ) : HostFrame

    /** From the thread of the solver of a node: the answer for a client */
    data class Response(
        val client: String,
        val message: String,
    ) : HostFrame

    /** From the thread of the solver of a node: what it asks its node (see Requests); [call] -1 if answered by nothing */
    data class Ask(
        val call: Int,
        val kind: String,
        val payload: String,
    ) : HostFrame

    /** To the thread of the solver of a node: the answer to what it asked */
    data class Reply(
        val call: Int,
        val payload: String,
    ) : HostFrame
}

/** The frames as text */
@InternalArg2pApi
object HostCodec {
    fun encode(frame: HostFrame): String =
        TextWriter()
            .apply {
                when (frame) {
                    is HostFrame.Start ->
                        text("Start")
                            .text(frame.session)
                            .number(frame.consumer)
                            .text(frame.start.theory)
                            .text(frame.start.flags)
                            .text(frame.start.goal)
                            .text(if (frame.start.interest == null) "0" else "1")
                            .texts(
                                frame.start.interest
                                    .orEmpty()
                                    .toList(),
                            ).text(if (frame.start.exploring) "1" else "0")
                    is HostFrame.Call ->
                        text("Call")
                            .text(frame.session)
                            .number(frame.consumer)
                            .number(frame.call)
                            .text(frame.kind)
                            .text(frame.payload)
                    is HostFrame.Answer ->
                        text("Answer")
                            .text(frame.session)
                            .number(frame.consumer)
                            .number(frame.call)
                            .text(frame.delivery)
                    is HostFrame.Abandon -> text("Abandon").text(frame.session).number(frame.consumer).text(frame.reason)
                    HostFrame.Done -> text("Done")
                    HostFrame.Stop -> text("Stop")
                    is HostFrame.Client -> text("Client").text(frame.message)
                    is HostFrame.Response -> text("Response").text(frame.client).text(frame.message)
                    is HostFrame.Ask -> text("Ask").number(frame.call).text(frame.kind).text(frame.payload)
                    is HostFrame.Reply -> text("Reply").number(frame.call).text(frame.payload)
                }
            }.toString()

    /** The frame of [text]: an IllegalArgumentException if it is none */
    fun decode(text: String): HostFrame =
        TextReader(text).run {
            when (val kind = text()) {
                "Start" -> {
                    val session = text()
                    val consumer = number()
                    val theory = text()
                    val flags = text()
                    val goal = text()
                    val some = text() == "1"
                    val interest = texts().toSet()
                    HostFrame.Start(session, consumer, ConsumerStart(theory, flags, goal, if (some) interest else null, text() == "1"))
                }
                "Call" -> HostFrame.Call(text(), number(), number(), text(), text())
                "Answer" -> HostFrame.Answer(text(), number(), number(), text())
                "Abandon" -> HostFrame.Abandon(text(), number(), text())
                "Done" -> HostFrame.Done
                "Stop" -> HostFrame.Stop
                "Client" -> HostFrame.Client(text())
                "Response" -> HostFrame.Response(text(), text())
                "Ask" -> HostFrame.Ask(number(), text(), text())
                "Reply" -> HostFrame.Reply(number(), text())
                else -> throw IllegalArgumentException("Unknown frame $kind")
            }
        }
}
