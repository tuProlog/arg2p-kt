package it.unibo.tuprolog.argumentation.parallel.store

/**
 * What a consumer sees of the store of its graph, whatever is between them. Every call that returns a [Delivery] waits, on
 * the thread of the consumer, for what the store answers.
 */
internal interface StoreConnection {
    /** Whether the store is in another process: the terms of a segment are published too */
    val remote: Boolean

    fun publish(entry: Entry)

    fun poll(): Delivery

    fun await(): Delivery

    fun barrier(at: Int): Delivery

    fun fail(reason: String)

    /** Every entry read again, from the first, the consumer's own too */
    fun rewind()
}

/** The consumer [id] of a store of this process: the segments read as they are, what the store answers waited for on a lock */
internal class DirectConnection(
    private val store: SharedStore,
    private val id: Int,
) : StoreConnection {
    override val remote: Boolean get() = store.remote

    override fun publish(entry: Entry) = store.publish(id, entry)

    override fun poll(): Delivery = store.poll(id)

    override fun await(): Delivery = waitFor { store.await(id, it) }

    override fun barrier(at: Int): Delivery = waitFor { store.barrier(id, at, it) }

    override fun fail(reason: String) = store.fail(reason)

    override fun rewind() = store.rewind(id)
}

/** The text a [MessageConnection] exchanges with its store, over whatever medium: see [StoreProtocol] */
internal interface StoreLine {
    /** [kind] with [payload] told to the store: nothing answers it */
    fun tell(
        kind: String,
        payload: String = "",
    )

    /** [kind] with [payload] asked of the store: its answer, a delivery as text, waited for */
    fun call(
        kind: String,
        payload: String = "",
    ): String
}

/** A consumer reaching its store through text: everything it publishes and is given written and read back */
internal class MessageConnection(
    private val line: StoreLine,
) : StoreConnection {
    override val remote: Boolean get() = true

    override fun publish(entry: Entry) = line.tell(StoreProtocol.PUBLISH, StoreProtocol.encode(entry.terms!!))

    override fun poll(): Delivery = StoreProtocol.decodeDelivery(line.call(StoreProtocol.POLL))

    override fun await(): Delivery = StoreProtocol.decodeDelivery(line.call(StoreProtocol.AWAIT))

    override fun barrier(at: Int): Delivery = StoreProtocol.decodeDelivery(line.call(StoreProtocol.BARRIER, at.toString()))

    override fun fail(reason: String) = line.tell(StoreProtocol.FAIL, reason)

    override fun rewind() = line.tell(StoreProtocol.REWIND)
}

/** The answer [start] gives its callback, waited for */
internal fun <T : Any> waitFor(start: ((T) -> Unit) -> Unit): T {
    val waiting = Waiting()
    var answer: T? = null
    start { value ->
        waiting.locked {
            answer = value
            waiting.wakeAll()
        }
    }
    return waiting.locked {
        while (answer == null) waiting.await()
        answer!!
    }
}
