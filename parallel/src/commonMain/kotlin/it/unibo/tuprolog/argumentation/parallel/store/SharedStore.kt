package it.unibo.tuprolog.argumentation.parallel.store

import it.unibo.tuprolog.argumentation.core.graph.GraphSegment
import it.unibo.tuprolog.argumentation.core.graph.SegmentTerms
import it.unibo.tuprolog.argumentation.parallel.host.locked

/**
 * What a consumer of a shared graph published: a [segment] of the solvers of its session, read as it is by those of the
 * same [session], and its [terms] (see SegmentTerms), for the consumers of other processes; at least one of them.
 */
internal class Entry(
    val origin: Int,
    val session: Any?,
    val segment: GraphSegment?,
    val terms: SegmentTerms?,
)

/** What a consumer is given: the entries of the others it had not read, and whether the phase is over, or why a consumer failed */
internal class Delivery(
    val entries: List<Entry>,
    val over: Boolean = false,
    val failure: String? = null,
)

/**
 * The state of a graph its [consumers] build together, wherever they run: the entries they published, in order, what each
 * read, the consumers waiting with nothing new (a phase is over once all of them are, every entry read), the barriers each
 * reached. It answers through callbacks, never waiting itself: how a consumer waits for the answer is up to its
 * connection (see [StoreConnection]).
 */
internal class SharedStore(
    private val consumers: Int,
) {
    private val lock = Any()
    private val entries = ArrayList<Entry>()

    private class Consumer {
        var read = 0
        var waiting: ((Delivery) -> Unit)? = null

        // reading every entry, its own too (see rewind)
        var all = false
    }

    private val known = HashMap<Int, Consumer>()
    private var over = false
    private val reached = ArrayList<Int>()
    private val atBarrier = ArrayList<MutableList<Pair<Int, (Delivery) -> Unit>>>()
    private var failure: String? = null
    private var lostHost = false

    /** Whether the graph failed as a host running some of its consumers left (see [fail]): one to build again */
    val lost: Boolean get() = locked(lock) { lostHost }

    // whether consumers of other processes read the entries
    private var remoteReaders = false

    /** Whether consumers of other processes read the entries: those of this one publish their terms too */
    var remote: Boolean
        get() = locked(lock) { remoteReaders }
        set(value) = locked(lock) { remoteReaders = value }

    private fun consumer(id: Int): Consumer = known.getOrPut(id) { Consumer() }

    /** Why a consumer failed, if one did: the graph is not built */
    val failed: String? get() = locked(lock) { failure }

    // (under the lock) the entries of the others [consumer] had not read, read (its own too if it reads them all)
    private fun unread(
        id: Int,
        consumer: Consumer,
    ): List<Entry> {
        val found = entries.subList(consumer.read, entries.size).filter { consumer.all || it.origin != id }
        consumer.read = entries.size
        return found
    }

    // (under the lock) every consumer waiting, every entry read
    private fun quiescent(): Boolean = known.size == consumers && known.values.all { it.waiting != null && it.read == entries.size }

    /** [entry] published by the consumer [id]: those waiting for one given what they had not read */
    fun publish(
        id: Int,
        entry: Entry,
    ) {
        val woken = ArrayList<Pair<(Delivery) -> Unit, Delivery>>()
        locked(lock) {
            entries += Entry(id, entry.session, entry.segment, entry.terms)
            consumer(id)
            for ((other, consumer) in known) {
                val reply = consumer.waiting ?: continue
                if (other == id) continue
                consumer.waiting = null
                woken += reply to Delivery(unread(other, consumer))
            }
        }
        woken.forEach { (reply, delivery) -> reply(delivery) }
    }

    /** The entries of the others the consumer [id] had not read */
    fun poll(id: Int): Delivery =
        locked(lock) {
            val failed = failure
            if (failed != null) Delivery(emptyList(), failure = failed) else Delivery(unread(id, consumer(id)))
        }

    /**
     * The consumer [id] reads every entry again, from the first, its own too: its solver reads anew the graph it built
     * (what it does again on it adds nothing)
     */
    fun rewind(id: Int) {
        locked(lock) {
            val consumer = consumer(id)
            consumer.read = 0
            consumer.all = true
        }
    }

    /**
     * The consumer [id] with nothing new: given what the others publish, as soon as they do, or that the phase is over once
     * every consumer waits, every entry read (all of them told at once)
     */
    fun await(
        id: Int,
        reply: (Delivery) -> Unit,
    ) {
        val replies = ArrayList<Pair<(Delivery) -> Unit, Delivery>>()
        locked(lock) {
            val consumer = consumer(id)
            when {
                failure != null -> replies += reply to Delivery(emptyList(), failure = failure)
                over -> replies += reply to Delivery(emptyList(), over = true)
                consumer.read < entries.size && entries.subList(consumer.read, entries.size).any { consumer.all || it.origin != id } ->
                    replies += reply to Delivery(unread(id, consumer))
                else -> {
                    consumer.read = entries.size
                    consumer.waiting = reply
                    if (quiescent()) {
                        over = true
                        for (waiting in known.values) {
                            replies += waiting.waiting!! to Delivery(emptyList(), over = true)
                            waiting.waiting = null
                        }
                    }
                }
            }
        }
        replies.forEach { (r, d) -> r(d) }
    }

    /** The consumer [id] at its barrier [at]: given what it had not read once every consumer reached it */
    fun barrier(
        id: Int,
        at: Int,
        reply: (Delivery) -> Unit,
    ) {
        val replies = ArrayList<Pair<(Delivery) -> Unit, Delivery>>()
        locked(lock) {
            consumer(id)
            // (the phase of the builders, once over, stays so: a solver building the graph again reads it as it is)
            while (reached.size <= at) {
                reached.add(0)
                atBarrier.add(mutableListOf())
            }
            reached[at]++
            atBarrier[at] += id to reply
            if (failure != null) {
                atBarrier[at].forEach { (_, r) -> replies += r to Delivery(emptyList(), failure = failure) }
                atBarrier[at].clear()
            } else if (reached[at] >= consumers) {
                atBarrier[at].forEach { (other, r) -> replies += r to Delivery(unread(other, consumer(other))) }
                atBarrier[at].clear()
            }
        }
        replies.forEach { (r, d) -> r(d) }
    }

    /** The arguments and the attacks each consumer published, by consumer */
    fun published(): Map<Int, Pair<Int, Int>> =
        locked(lock) {
            entries.groupBy { it.origin }.mapValues { (_, published) ->
                published.sumOf { it.segment?.arguments?.size ?: it.terms!!.arguments.size } to
                    published.sumOf { it.segment?.attacks?.size ?: it.terms!!.attacks.size }
            }
        }

    /** A consumer failed, for [reason] (a host running some [lost]): those waiting, and those that will, are told */
    fun fail(
        reason: String,
        lost: Boolean = false,
    ) {
        val replies = ArrayList<Pair<(Delivery) -> Unit, Delivery>>()
        locked(lock) {
            if (failure == null) {
                failure = reason
                lostHost = lost
            }
            for (consumer in known.values) {
                consumer.waiting?.let { replies += it to Delivery(emptyList(), failure = reason) }
                consumer.waiting = null
            }
            for (waiting in atBarrier) {
                waiting.forEach { (_, r) -> replies += r to Delivery(emptyList(), failure = reason) }
                waiting.clear()
            }
        }
        replies.forEach { (r, d) -> r(d) }
    }
}
