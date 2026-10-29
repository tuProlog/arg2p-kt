package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.js.Promise

/**
 * The links of [site] over WebSockets, to sites on any platform (JVM, Node.js, browsers): the links it accepts once
 * it [listen]s (on Node.js, through the package ws), and those it opens ([connect], with the WebSocket of the
 * runtime). Each frame is a text message; those of a link arrive in order, and one longer than a few tens of megabytes
 * closes a link it accepted (the WebSocket of a browser bounds none). The failures of a link, and those of the handling
 * of its frames, are reported on the console.
 *
 * Unlike the links of the JVM, the port listened on serves no pages: a plain HTTP request is refused (426, by ws).
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
            options.maxPayload = LONGEST_FRAME
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
                socket.on("error") { error: dynamic -> console.error(error) }
                handled { site.opened(link) }
            }
        }

    /**
     * Opens a link to the site listening at [url] (ws://host:port), waiting for it at most [timeoutMillis]: [code]
     * proven if given (see [Site.requiresCode]), and the link given to [opening] before it opens; rejected if it does
     * not open in time
     */
    fun connect(
        url: String,
        code: String? = null,
        timeoutMillis: Int = 10_000,
        opening: (Link) -> Unit = {},
    ): Promise<Unit> =
        Promise { resolve, reject ->
            val constructor = js("globalThis.WebSocket") ?: nodeModule("ws").WebSocket
            val socket = js("new constructor(url)")
            val link = SocketLink(socket, outgoing = true, code = code)
            opening(link)
            var open = false
            // not open in time: closed, which rejects
            later(timeoutMillis) { if (!open) socket.close() }
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
            // an error closes the socket (onclose follows); not one of a site gone, or not there
            socket.onerror = { error: dynamic -> if (open) console.error(error) }
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

// [task] run in [millis] milliseconds: the timer does not keep Node.js running
internal fun later(
    millis: Int,
    task: () -> Unit,
) {
    val timer = js("setTimeout(task, millis)")
    // a number in browsers, a Timeout in Node.js
    if (jsTypeOf(timer.unref) == "function") timer.unref()
}
