package it.unibo.tuprolog.argumentation.parallel.store

import it.unibo.tuprolog.argumentation.core.graph.AttackRef
import it.unibo.tuprolog.argumentation.core.graph.SegmentTerms
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

/**
 * How a store is reached through text, over any medium: the calls of a consumer ([PUBLISH], [POLL], ...), each answered by
 * nothing or by a delivery, and the segments as text (the terms as Prolog text, read again with the operators of arg2p,
 * the positions as numbers). A store serves the calls with [serve].
 */
internal object StoreProtocol {
    const val PUBLISH = "publish"
    const val POLL = "poll"
    const val AWAIT = "await"
    const val BARRIER = "barrier"
    const val FAIL = "fail"
    const val REWIND = "rewind"

    /**
     * The call of the consumer [consumer], [kind] with [payload], made on [store]: its answer, as text, given to [answer]
     * (none for those answered by nothing). Called on any thread: the store never waits.
     */
    fun serve(
        store: SharedStore,
        consumer: Int,
        kind: String,
        payload: String,
        answer: (String) -> Unit,
    ) {
        val reply = { delivery: Delivery -> answer(encode(delivery)) }
        when (kind) {
            PUBLISH -> store.publish(consumer, Entry(consumer, null, null, decode(payload)))
            POLL -> reply(store.poll(consumer))
            AWAIT -> store.await(consumer, reply)
            BARRIER -> store.barrier(consumer, payload.toInt(), reply)
            FAIL -> store.fail(payload)
            REWIND -> store.rewind(consumer)
        }
    }

    /** The answer telling a consumer the graph failed, for [reason] */
    fun failure(reason: String): String = encode(Delivery(emptyList(), failure = reason))

    fun encode(segment: SegmentTerms): String =
        TextWriter()
            .terms(segment.refs)
            .numbers(segment.arguments)
            .terms(segment.infos)
            .numbers(segment.supports)
            .attacks(segment.attacks)
            .attacks(segment.removed)
            .terms(segment.explored)
            .toString()

    fun decode(text: String): SegmentTerms =
        TextReader(text).run {
            SegmentTerms(terms(), numbers(), terms(), numbers(), attacks(), attacks(), terms())
        }

    /** [delivery] as text: each entry its origin and its terms */
    fun encode(delivery: Delivery): String =
        TextWriter()
            .list(delivery.entries) { number(it.origin).text(encode(it.terms!!)) }
            .text(if (delivery.over) "1" else "0")
            .texts(listOfNotNull(delivery.failure))
            .toString()

    /** The delivery of [text] (see [encode]): its entries with their terms alone */
    fun decodeDelivery(text: String): Delivery =
        TextReader(text).run {
            val entries = list { Entry(number(), null, null, decode(text())) }
            Delivery(entries, text() == "1", texts().singleOrNull())
        }

    private fun TextWriter.terms(values: List<Term>) = list(values) { text(it.toString()) }

    private fun TextWriter.numbers(values: IntArray) = list(values.toList()) { number(it) }

    private fun TextWriter.attacks(values: List<AttackRef>) =
        list(values) { text(it.type).number(it.attacker).number(it.attacked).number(it.on) }

    private fun TextReader.terms() = list { Term.parse(text(), TermValues.operators) }

    private fun TextReader.numbers() = list { number() }.toIntArray()

    private fun TextReader.attacks() = list { AttackRef(text(), number(), number(), number()) }
}

/**
 * A line to a store of this process, written and read back as one to another process would be, for the tests of what
 * goes between processes
 */
internal class LoopbackLine(
    private val store: SharedStore,
    private val consumer: Int,
) : StoreLine {
    override fun tell(
        kind: String,
        payload: String,
    ) = StoreProtocol.serve(store, consumer, kind, payload) {}

    override fun call(
        kind: String,
        payload: String,
    ): String = waitFor { StoreProtocol.serve(store, consumer, kind, payload, it) }
}
