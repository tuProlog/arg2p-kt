package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * A connection between two sites, over any transport (memory, the threads of a process, a socket): the frames sent on
 * it arrive in the order they were sent, until it is closed
 */
interface Link {
    /** Sends [frame] to the site at the other end, if the link is still open */
    fun send(frame: String)

    /** Closes the link: both ends are told (see [LinkListener.closed]) */
    fun close()
}

/** A site, as the transports see it: told of its links as they open, of the frames on them and of their closing */
interface LinkListener {
    fun opened(link: Link)

    fun received(
        link: Link,
        frame: String,
    )

    fun closed(link: Link)
}

/** Links between sites of the same process, for tests and for sites sharing a machine without a network between them */
object MemoryLinks {
    /**
     * A link between [first] and [second], each told of its end: the frames are delivered by [via] if given, on the
     * thread of the sender otherwise
     */
    fun connect(
        first: LinkListener,
        second: LinkListener,
        via: TaskQueue? = null,
    ) {
        val one = End(first, via)
        val other = End(second, via)
        one.other = other
        other.other = one
        first.opened(one)
        second.opened(other)
    }

    private class End(
        private val listener: LinkListener,
        private val via: TaskQueue?,
    ) : Link {
        lateinit var other: End

        @kotlin.concurrent.Volatile
        private var open = true

        override fun send(frame: String) {
            if (open) run { other.listener.received(other, frame) }
        }

        override fun close() {
            if (!open) return
            open = false
            other.open = false
            run {
                listener.closed(this)
                other.listener.closed(other)
            }
        }

        private fun run(task: () -> Unit) = via?.post(task) ?: task()
    }
}
