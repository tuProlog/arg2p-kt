package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.federation.Link
import it.unibo.tuprolog.argumentation.parallel.federation.Site
import it.unibo.tuprolog.argumentation.parallel.federation.handled
import kotlin.js.Promise

/**
 * The replies of a site whose workers run on [threads] threads of its own (its delegates, see [Site.delegate]), asked
 * for once every thread greeted it: [running] while some are pending (the threads keeping Node.js running then), and
 * rejected if pending when the cluster closes
 */
internal class ThreadReplies(
    private val threads: Int,
    private val running: (Boolean) -> Unit = {},
) : AsyncReplies {
    // The evaluations asked for before every thread greeted the site, and the replies pending
    private val waiting = mutableListOf<() -> Unit>()
    private val pending = mutableSetOf<Pending>()
    private var greeted = 0
    private var closed = false

    private class Pending(
        val reject: (Throwable) -> Unit,
    )

    /** The frames of a thread for [site], on [link]: the thread greeted the site once it is admitted */
    fun receiver(
        site: Site,
        link: Link,
    ): (String) -> Unit {
        var admitted = false
        return { frame ->
            handled { site.received(link, frame) }
            if (!admitted && site.admitted(link)) {
                admitted = true
                if (++greeted == threads) waiting.toList().also { waiting.clear() }.forEach { it() }
            }
        }
    }

    override fun <T> replied(ask: ((T) -> Unit) -> Unit): Promise<T> =
        Promise { resolve, reject ->
            if (closed) return@Promise reject(closedCluster())
            val asked = Pending(reject)
            if (pending.isEmpty()) running(true)
            pending += asked
            val evaluate = {
                ask {
                    if (pending.remove(asked)) {
                        resolve(it)
                        if (pending.isEmpty()) running(false)
                    }
                }
            }
            if (greeted == threads) evaluate() else waiting += evaluate
        }

    /** The replies pending rejected, and those asked for from now on */
    fun close() {
        closed = true
        waiting.clear()
        pending.toList().also { pending.clear() }.forEach { it.reject(closedCluster()) }
    }

    private fun closedCluster() = Arg2pException("The cluster is closed")
}
