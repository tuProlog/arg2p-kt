@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.NodeService
import it.unibo.tuprolog.argumentation.parallel.ParallelNode
import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter
import kotlin.js.Promise

/*
 * A federation in JavaScript: its site on the thread of the node of the parallel solver, which never waits (see FederationService);
 * the Federation itself on the thread that made it, reaching its site through text.
 */

/** The messages between a Federation and its site, as text */
internal sealed interface FederationConsole {
    /** To the site, first: what it is */
    data class Open(
        val name: String,
        val lends: Boolean,
    ) : FederationConsole

    /** To the site: [method] with [args], answered by a [Reply] to [call] */
    data class Call(
        val call: Int,
        val method: String,
        val args: List<String>,
    ) : FederationConsole

    data class Reply(
        val call: Int,
        val failure: String?,
        val payload: String,
    ) : FederationConsole

    /** To the Federation: the state of its site, sent before each reply and as it changes */
    data class State(
        val peers: List<String>,
        val code: String,
        val stats: String,
    ) : FederationConsole

    /** To the Federation: the peers changed */
    data object PeersChanged : FederationConsole

    /** To the site: a link the Federation keeps for it (over a medium of the page) opened, known as [link] */
    data class LinkOpened(
        val link: Int,
        val outgoing: Boolean,
        val code: String?,
        val address: String?,
    ) : FederationConsole

    /** To the site: [frame] on the link [link]; to the Federation: [frame] to send on it */
    data class LinkFrame(
        val link: Int,
        val frame: String,
    ) : FederationConsole

    /** To the site: the link [link] closed; to the Federation: to close it */
    data class LinkClosed(
        val link: Int,
    ) : FederationConsole

    companion object {
        fun encode(message: FederationConsole): String =
            TextWriter()
                .apply {
                    when (message) {
                        is Open -> text("Open").text(message.name).text(if (message.lends) "1" else "0")
                        is Call -> text("Call").number(message.call).text(message.method).texts(message.args)
                        is Reply -> text("Reply").number(message.call).texts(listOfNotNull(message.failure)).text(message.payload)
                        is State ->
                            text("State")
                                .texts(message.peers)
                                .text(message.code)
                                .text(message.stats)
                        PeersChanged -> text("PeersChanged")
                        is LinkOpened ->
                            text("LinkOpened")
                                .number(message.link)
                                .text(if (message.outgoing) "1" else "0")
                                .texts(listOfNotNull(message.code))
                                .texts(listOfNotNull(message.address))
                        is LinkFrame -> text("LinkFrame").number(message.link).text(message.frame)
                        is LinkClosed -> text("LinkClosed").number(message.link)
                    }
                }.toString()

        fun decode(text: String): FederationConsole =
            TextReader(text).run {
                when (val kind = text()) {
                    "Open" -> Open(text(), text() == "1")
                    "Call" -> Call(number(), text(), texts())
                    "Reply" -> Reply(number(), texts().singleOrNull(), text())
                    "State" -> State(texts(), text(), text())
                    "PeersChanged" -> PeersChanged
                    "LinkOpened" -> LinkOpened(number(), text() == "1", texts().singleOrNull(), texts().singleOrNull())
                    "LinkFrame" -> LinkFrame(number(), text())
                    "LinkClosed" -> LinkClosed(number())
                    else -> throw IllegalArgumentException("Unknown message $kind")
                }
            }
    }
}

/** The methods a Federation calls on its site */
internal object FederationMethods {
    const val NAME = "name"
    const val REQUIRES_CODE = "requiresCode"
    const val NEW_CODE = "newCode"
    const val LISTEN = "listen"
    const val CONNECT = "connect"
    const val CLOSE = "close"
}

/**
 * Makes what stands on the node of a parallel solver for a [Federation] to work (see [NodeService]): for the threads of a parallel solver to
 * be given, as `startWebWorker(nodeService)` in the script of its Web Workers
 */
@JsExport
fun nodeService(
    node: dynamic,
    send: dynamic,
): dynamic = FederationService(node as ParallelNode, send.unsafeCast<(String) -> Unit>())

/**
 * The site of a parallel solver on the thread of its node, served to its [Federation]: the calls answered by [send], the state of the
 * site sent before each answer and as it changes
 */
internal class FederationService(
    private val node: ParallelNode,
    private val send: (String) -> Unit,
) : NodeService {
    private var site: Site? = null
    private var links: WebSocketLinks? = null

    // The links its Federation keeps for it, by the number it gave them
    private val kept = HashMap<Int, KeptLink>()

    private inner class KeptLink(
        private val number: Int,
        override val outgoing: Boolean,
        override val code: String?,
        override val address: String?,
    ) : Link {
        override fun send(frame: String) = tell(FederationConsole.LinkFrame(number, frame))

        override fun close() = tell(FederationConsole.LinkClosed(number))
    }

    private fun tell(message: FederationConsole) = send(FederationConsole.encode(message))

    private fun open(message: FederationConsole.Open) {
        val made = Site(node, message.lends)
        made.name = message.name
        site = made
        links = WebSocketLinks(made)
        made.onPeersChanged = {
            state()
            tell(FederationConsole.PeersChanged)
        }
        state()
    }

    private fun state() {
        val site = site ?: return
        tell(FederationConsole.State(site.peerSites.toList(), site.code, StatsCodec.encode(site.stats)))
    }

    private fun reply(
        call: Int,
        payload: String = "",
        failure: String? = null,
    ) {
        state()
        tell(FederationConsole.Reply(call, failure, payload))
    }

    private fun <T> replied(
        call: Int,
        promise: Promise<T>,
    ) {
        promise.then({ reply(call, it?.toString() ?: "") }, { reply(call, failure = it.message ?: it.toString()) })
    }

    override fun received(text: String) {
        val decoded = FederationConsole.decode(text)
        if (decoded is FederationConsole.Open) return open(decoded)
        val site = site ?: return
        val links = links!!
        val message =
            when (decoded) {
                is FederationConsole.Call -> decoded
                is FederationConsole.LinkOpened -> {
                    val link = KeptLink(decoded.link, decoded.outgoing, decoded.code, decoded.address)
                    kept[decoded.link] = link
                    return site.opened(link)
                }
                is FederationConsole.LinkFrame -> return kept[decoded.link]?.let { site.received(it, decoded.frame) } ?: Unit
                is FederationConsole.LinkClosed -> return kept.remove(decoded.link)?.let { site.closed(it) } ?: Unit
                else -> return
            }
        val (call, method, args) = message
        when (method) {
            FederationMethods.NAME -> reply(call).also { site.name = args[0] }
            FederationMethods.REQUIRES_CODE -> reply(call).also { site.requiresCode = args[0] == "1" }
            FederationMethods.NEW_CODE -> reply(call, site.newCode(args[0]))
            FederationMethods.LISTEN -> replied(call, links.listen(args[0].toInt()))
            FederationMethods.CONNECT -> replied(call, connect(site, links, args[0], args.getOrNull(1)))
            FederationMethods.CLOSE -> {
                close()
                reply(call)
            }
        }
    }

    override fun close() {
        links?.close()
    }

    // See Federation.connect
    private fun connect(
        site: Site,
        links: WebSocketLinks,
        url: String,
        code: String?,
    ): Promise<Unit> =
        Promise { resolve, reject ->
            var opened: Link? = null
            var done = false
            // the link greeted, refused or closed, or not in time: the greeting no longer waited for then
            val settle = { refusal: String? ->
                if (!done) {
                    done = true
                    opened?.let { site.forgetGreeting(it) }
                    if (refusal == null) resolve(Unit) else reject(Arg2pException(refusal))
                }
            }
            later(WAITING) {
                if (!done) opened?.close()
                settle("$url did not answer in time")
            }
            links
                .connect(url, site.secretFor(url, code), WAITING) { link ->
                    opened = link
                    site.linking(url, link) { refusal -> settle(refusal?.let { "$url refused the link: $it" }) }
                }.catch { settle("$url cannot be reached: ${it.message}") }
        }

    private companion object {
        // How long a link waits for the other site, in milliseconds
        const val WAITING = 10_000
    }
}
