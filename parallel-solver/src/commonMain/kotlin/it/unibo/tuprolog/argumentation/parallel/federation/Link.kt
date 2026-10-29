package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * A connection between two sites, over any transport (memory, the threads of a process, a socket): the frames sent on
 * it arrive in the order they were sent, until it is closed
 */
interface Link {
    /** Sends [frame] to the site at the other end, if the link is still open */
    fun send(frame: String)

    /** Whether this site opened the link: a site requiring a code (see [Site.requiresCode]) checks the links it did not */
    val outgoing: Boolean get() = false

    /** The code this site presents on a link it opened, to a site requiring one: none by default */
    val code: String? get() = null

    /** The address of the other end, if known: the wrong codes of an address are refused for a while, after a few */
    val address: String? get() = null

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
    ) = link(first, second, via) { one, _ -> first.opened(one) }

    /** A link between [site] and [delegate], a delegate of it (see [Site.delegate]), as [connect] makes it */
    fun delegate(
        site: Site,
        delegate: LinkListener,
        via: TaskQueue? = null,
    ) = link(site, delegate, via) { one, _ -> site.delegate(one) }

    private fun link(
        first: LinkListener,
        second: LinkListener,
        via: TaskQueue?,
        open: (Link, Link) -> Unit,
    ) {
        val one = End(first, via)
        val other = End(second, via)
        one.other = other
        other.other = one
        open(one, other)
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
