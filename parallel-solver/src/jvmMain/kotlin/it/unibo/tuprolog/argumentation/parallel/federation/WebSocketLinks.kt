package it.unibo.tuprolog.argumentation.parallel.federation

import org.java_websocket.WebSocket
import org.java_websocket.client.WebSocketClient
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.handshake.ServerHandshake
import org.java_websocket.server.WebSocketServer
import java.net.InetSocketAddress
import java.net.URI
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

/**
 * The links of [site] over WebSockets, to sites on any platform (JVM, Node.js, browsers): the links it accepts once
 * it [listen]s, and those it opens ([connect]). Each frame is a text message; those of a link arrive in order.
 */
class WebSocketLinks(
    private val site: LinkListener,
) : AutoCloseable {
    private var server: Server? = null
    private val clients = ConcurrentHashMap.newKeySet<Client>()

    /** Accepts the links of other sites on [port] of [host] (an ephemeral port for 0): the port taken */
    fun listen(
        port: Int = 0,
        host: String = "0.0.0.0",
    ): Int {
        check(server == null) { "Already listening" }
        val started = Server(InetSocketAddress(host, port)).also { it.start() }
        started.started.await()
        started.failure?.let { throw it }
        server = started
        return started.port
    }

    /** Opens a link to the site listening at [uri] (ws://host:port), waiting for it at most [timeoutMillis] */
    fun connect(
        uri: String,
        timeoutMillis: Long = 10_000,
    ) {
        val client = Client(URI(uri))
        clients += client
        check(client.connectBlocking(timeoutMillis, TimeUnit.MILLISECONDS)) { "No site at $uri" }
    }

    /** Closes every link, and stops listening */
    override fun close() {
        clients.forEach { it.closeBlocking() }
        server?.stop()
        server = null
    }

    // A link on an open WebSocket: sends from several threads, one at a time
    private class SocketLink(
        private val socket: WebSocket,
    ) : Link {
        override fun send(frame: String) {
            synchronized(this) { if (socket.isOpen) socket.send(frame) }
        }

        override fun close() = socket.close()
    }

    private inner class Server(
        address: InetSocketAddress,
    ) : WebSocketServer(address) {
        val started = CountDownLatch(1)
        var failure: Exception? = null
        private val links = ConcurrentHashMap<WebSocket, SocketLink>()

        init {
            isReuseAddr = true
        }

        override fun onStart() = started.countDown()

        override fun onOpen(
            socket: WebSocket,
            handshake: ClientHandshake,
        ) = site.opened(SocketLink(socket).also { links[socket] = it })

        override fun onMessage(
            socket: WebSocket,
            message: String,
        ) = links[socket]?.let { site.received(it, message) } ?: Unit

        override fun onClose(
            socket: WebSocket,
            code: Int,
            reason: String,
            remote: Boolean,
        ) = links.remove(socket)?.let { site.closed(it) } ?: Unit

        override fun onError(
            socket: WebSocket?,
            error: Exception,
        ) {
            // an error on a link closes it (onClose follows), one of the server stops it
            if (socket == null) {
                failure = error
                started.countDown()
            }
        }
    }

    private inner class Client(
        uri: URI,
    ) : WebSocketClient(uri) {
        private val link = SocketLink(this)

        override fun onOpen(handshake: ServerHandshake) = site.opened(link)

        override fun onMessage(message: String) = site.received(link, message)

        override fun onClose(
            code: Int,
            reason: String,
            remote: Boolean,
        ) {
            clients -= this
            site.closed(link)
        }

        override fun onError(error: Exception) = Unit
    }
}
