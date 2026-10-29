package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.js.Promise

/**
 * The links of [site] over WebSockets, to sites on any platform (JVM, Node.js, browsers): the links it accepts once
 * it [listen]s (on Node.js, through the package ws), and those it opens ([connect], with the WebSocket of the
 * runtime). Each frame is a text message; those of a link arrive in order.
 */
class WebSocketLinks(
    private val site: LinkListener,
) {
    private var server: dynamic = null
    private val sockets = mutableListOf<dynamic>()

    /** Accepts the links of other sites on [port] of [host] (an ephemeral port for 0), on Node.js: the port taken */
    fun listen(
        port: Int = 0,
        host: String = "0.0.0.0",
    ): Promise<Int> =
        Promise { resolve, reject ->
            check(server == null) { "Already listening" }
            val options: dynamic = js("({})")
            options.port = port
            options.host = host
            val constructor = nodeModule("ws").WebSocketServer
            val started = js("new constructor(options)")
            started.on("listening") { _: dynamic ->
                server = started
                resolve(started.address().port as Int)
            }
            started.on("error") { error: dynamic -> reject(error as Throwable) }
            started.on("connection") { socket: dynamic, request: dynamic ->
                val link = SocketLink(socket, address = request.socket.remoteAddress as String?)
                sockets.add(socket)
                // the text of the Buffer itself (Kotlin's toString would write its bytes)
                socket.on("message") { data: dynamic -> handled { site.received(link, data.toString("utf8") as String) } }
                socket.on("close") { _: dynamic ->
                    sockets.remove(socket)
                    handled { site.closed(link) }
                }
                socket.on("error") { _: dynamic -> Unit }
                handled { site.opened(link) }
            }
        }

    /**
     * Opens a link to the site listening at [url] (ws://host:port): [code] presented if given (see
     * [Site.requiresCode]), and the link given to [opening] before it opens
     */
    fun connect(
        url: String,
        code: String? = null,
        opening: (Link) -> Unit = {},
    ): Promise<Unit> =
        Promise { resolve, reject ->
            val constructor = js("globalThis.WebSocket") ?: nodeModule("ws").WebSocket
            val socket = js("new constructor(url)")
            val link = SocketLink(socket, outgoing = true, code = code)
            opening(link)
            var open = false
            socket.onopen = { _: dynamic ->
                open = true
                sockets.add(socket)
                handled { site.opened(link) }
                resolve(Unit)
            }
            socket.onmessage = { event: dynamic -> handled { site.received(link, event.data as String) } }
            socket.onclose = { _: dynamic ->
                if (open) {
                    sockets.remove(socket)
                    handled { site.closed(link) }
                } else {
                    reject(IllegalStateException("No site at $url"))
                }
            }
            socket.onerror = { _: dynamic -> Unit }
        }

    /** Closes every link, and stops listening */
    fun close() {
        sockets.toList().forEach { it.close() }
        server?.close()
        server = null
    }

    // A link on a WebSocket, of the runtime or of the package ws: frames sent while it is open
    private class SocketLink(
        private val socket: dynamic,
        override val outgoing: Boolean = false,
        override val code: String? = null,
        override val address: String? = null,
    ) : Link {
        override fun send(frame: String) {
            if (socket.readyState == 1) socket.send(frame)
        }

        override fun close() {
            socket.close()
        }
    }
}

// A frame whose handling fails is skipped, as an actor resumed on a failure does
internal fun handled(block: () -> Unit) = runCatching(block).onFailure { console.error(it) }
