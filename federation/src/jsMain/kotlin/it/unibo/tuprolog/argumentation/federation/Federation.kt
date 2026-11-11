@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.ExperimentalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.ThreadSolver
import it.unibo.tuprolog.argumentation.parallel.host.nodeModule
import it.unibo.tuprolog.argumentation.parallel.onNode
import kotlin.js.JsExport
import kotlin.js.Promise

/**
 * A parallel solver of JavaScript federated with others, on the JVM or on Node.js, over WebSockets ([connect], and [listen] on Node.js),
 * or over any other medium ([linkOpened]). It runs consumers of the graphs of the others, and places the consumers of its graphs
 * on them, if it [lends]; a parallel solver on a page not isolated across origins runs none, and solves alone (see
 * [it.unibo.tuprolog.argumentation.parallel.WebWorkerSolver]).
 *
 * In a browser, the script of the Web Workers of [parallel] calls `startWebWorker(nodeService)`.
 */
@JsExport
@ExperimentalArg2pApi
class Federation(
    private val parallel: ThreadSolver,
    lends: Boolean = true,
) {
    private var calls = 0
    private val pending = HashMap<Int, Pair<(String) -> Unit, (String) -> Unit>>()
    private var lastPeers: List<String> = emptyList()
    private var lastCode = ""
    private var lastStats = FederationStats(parallel.id, emptyList())
    private var refusal: String? = null

    // How to tell the site of this federation something, on the thread of the node
    private val tell: (String) -> Unit = parallel.node.service(::received, ::refused)

    private fun tell(message: FederationConsole) = tell(FederationConsole.encode(message))

    /** Called whenever a site joins or leaves the peers of the parallel solver */
    var onPeersChanged: (() -> Unit)? = null

    // The links kept here for the site, on the thread of the node (those of a medium of the page)
    private val links = HashMap<Int, Link>()
    private val numbers = HashMap<Link, Int>()
    private var linksMade = 0

    init {
        tell(FederationConsole.Open(if (onNode()) nodeModule("os").hostname() as String else "", lends))
    }

    private fun refused(reason: String) {
        refusal = reason
        val waiting = pending.values.toList()
        pending.clear()
        waiting.forEach { (_, failed) ->
            parallel.node.release()
            failed(reason)
        }
    }

    private fun received(text: String) {
        when (val message = FederationConsole.decode(text)) {
            is FederationConsole.State -> {
                lastPeers = message.peers
                lastCode = message.code
                lastStats = StatsCodec.decodeFederation(message.stats)
            }
            FederationConsole.PeersChanged -> onPeersChanged?.invoke()
            is FederationConsole.Reply -> {
                val (done, failed) = pending.remove(message.call) ?: return
                parallel.node.release()
                if (message.failure == null) done(message.payload) else failed(message.failure)
            }
            is FederationConsole.LinkFrame -> links[message.link]?.send(message.frame)
            is FederationConsole.LinkClosed -> links[message.link]?.close()
            else -> Unit
        }
    }

    private fun request(
        method: String,
        vararg args: String?,
    ): Promise<String> =
        Promise { resolve, reject ->
            val refused = refusal
            if (refused != null) return@Promise reject(Arg2pException(refused))
            val number = calls++
            parallel.node.hold()
            pending[number] = { payload: String -> resolve(payload) } to { reason: String -> reject(Arg2pException(reason)) }
            tell(FederationConsole.Call(number, method, args.filterNotNull()))
        }

    // (a call whose answer nothing waits for)
    private fun told(
        method: String,
        vararg args: String,
    ) {
        request(method, *args).catch { console.error(it) }
    }

    /** The name of the site of the parallel solver, as its peers are told: on Node.js the name of its host by default */
    var name: String = if (onNode()) nodeModule("os").hostname() as String else ""
        set(value) {
            field = value
            told(FederationMethods.NAME, value)
        }

    /**
     * Whether the sites linking to this parallel solver must prove they know its [code], or the token of a pairing made with it:
     * refused otherwise, and after a few wrong codes from an address its codes are refused for a while
     */
    var requiresCode: Boolean = false
        set(value) {
            field = value
            told(FederationMethods.REQUIRES_CODE, if (value) "1" else "0")
        }

    /** The code the sites linking to this parallel solver prove they know, if it requires one: four digits, renewed once a site pairs with it */
    val code: String get() = lastCode

    /** A new [code], chosen here and told to the site, the one before refused from now on (the pairings made stay) */
    fun newCode(): String = Pairing.newCode().also { lastCode = it }.also { told(FederationMethods.NEW_CODE, it) }

    /** The sites the parallel solver is federated with */
    val peers: Array<String> get() = lastPeers.toTypedArray()

    /** The federation, as the parallel solver knows it: the sites */
    val stats: FederationStats get() = lastStats

    /**
     * Accepts the links of other sites (on any platform) over WebSockets, on [port] of [host] (an ephemeral port for 0): the port
     * taken. Node.js alone.
     */
    fun listen(
        port: Int = 0,
        host: String = "0.0.0.0",
    ): Promise<Int> = request(FederationMethods.LISTEN, port.toString(), host).then { it.toInt() }

    /**
     * Federates the parallel solver with the site listening at [url] (ws://host:port), once it admits this one: proving it knows
     * [code] if it requires one, the two paired then, and linked again to the same address with no code while they run.
     * Rejected if it refuses the code, does not prove it knows it as well (a site claiming the identifier of another), or does
     * not answer in a few seconds.
     */
    fun connect(
        url: String,
        code: String? = null,
    ): Promise<Unit> = request(FederationMethods.CONNECT, url, code).then { }

    /** [link], over a medium of the page, opened: the two sites greet each other on it */
    fun linkOpened(link: ExternalLink) {
        val number = linksMade++
        links[number] = link
        numbers[link] = number
        tell(FederationConsole.LinkOpened(number, link.outgoing, link.code, link.address))
    }

    /** [frame], come over [link] */
    fun linkReceived(
        link: ExternalLink,
        frame: String,
    ) {
        numbers[link]?.let { tell(FederationConsole.LinkFrame(it, frame)) }
    }

    /** [link] closed */
    fun linkClosed(link: ExternalLink) {
        val number = numbers.remove(link) ?: return
        links.remove(number)
        tell(FederationConsole.LinkClosed(number))
    }

    /** Closes the links to the other sites, those over a medium of the page too: the parallel solver goes on, alone */
    fun close(): Promise<Unit> =
        request(FederationMethods.CLOSE).then {
            numbers.keys.toList().forEach { it.close() }
        }
}
