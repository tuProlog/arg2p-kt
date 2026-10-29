package it.unibo.tuprolog.argumentation.parallel.federation

import org.java_websocket.WebSocket
import org.java_websocket.WebSocketImpl
import org.java_websocket.client.WebSocketClient
import org.java_websocket.drafts.Draft
import org.java_websocket.drafts.Draft_6455
import org.java_websocket.enums.HandshakeState
import org.java_websocket.framing.CloseFrame
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.handshake.HandshakeBuilder
import org.java_websocket.handshake.Handshakedata
import org.java_websocket.handshake.ServerHandshake
import org.java_websocket.handshake.ServerHandshakeBuilder
import org.java_websocket.server.WebSocketServer
import java.net.InetSocketAddress
import java.net.URI
import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

/**
 * The links of [site] over WebSockets, to sites on any platform (JVM, Node.js, browsers): the links it accepts once
 * it [listen]s, and those it opens ([connect]). Each frame is a text message; those of a link arrive in order. The port
 * listened on answers the plain HTTP requests for the [pages] it serves as well.
 */
class WebSocketLinks(
    private val site: LinkListener,
) : AutoCloseable {
    private var server: Server? = null
    private val clients = ConcurrentHashMap.newKeySet<Client>()

    /**
     * The page a plain HTTP GET of a path (as `/join?x=1`) is answered with, on the port listened on: none (null) for
     * a request refused (404), as for every other request but those opening a WebSocket. Asked as each request comes.
     */
    @Volatile var pages: (path: String) -> StaticContent? = { null }

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

    /**
     * Opens a link to the site listening at [uri] (ws://host:port), waiting for it at most [timeoutMillis]: [code]
     * presented if given (see [Site.requiresCode]), and the link given to [opening] before it opens
     */
    @JvmOverloads
    fun connect(
        uri: String,
        timeoutMillis: Long = 10_000,
        code: String? = null,
        opening: (Link) -> Unit = {},
    ) {
        val client = Client(URI(uri), code)
        clients += client
        opening(client.link)
        // opened, if closed at once since (a site refusing it)
        check(client.connectBlocking(timeoutMillis, TimeUnit.MILLISECONDS) || client.opened) { "No site at $uri" }
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
        override val outgoing: Boolean = false,
        override val code: String? = null,
    ) : Link {
        override val address: String? get() = socket.remoteSocketAddress?.address?.hostAddress

        override fun send(frame: String) {
            synchronized(this) { if (socket.isOpen) socket.send(frame) }
        }

        override fun close() = socket.close()
    }

    // The plain HTTP GETs of the pages served, answered as a WebSocket handshake would be, by another draft: the
    // requests opening a WebSocket are left to the draft of the WebSockets
    private inner class Pages(
        private var page: StaticContent? = null,
    ) : Draft_6455() {
        override fun acceptHandshakeAsServer(handshake: ClientHandshake): HandshakeState {
            // a WebSocket, unlike other upgrades (h2c), which are ignored
            if (handshake.getFieldValue("Upgrade").equals("websocket", ignoreCase = true)) return HandshakeState.NOT_MATCHED
            page = pages(handshake.resourceDescriptor)
            return if (page == null) HandshakeState.NOT_MATCHED else HandshakeState.MATCHED
        }

        override fun postProcessHandshakeResponseAsServer(
            request: ClientHandshake,
            response: ServerHandshakeBuilder,
        ): HandshakeBuilder =
            response.apply {
                val served = page!!
                put("Content-Type", served.contentType)
                put("Content-Length", served.content.size.toString())
                put("Cache-Control", "no-store")
                put("Connection", "close")
                content = served.content
            }

        // the response of a page, rather than the one switching to a WebSocket
        override fun createHandshake(
            handshake: Handshakedata,
            withContent: Boolean,
        ): List<ByteBuffer> {
            val head =
                handshake.iterateHttpFields().asSequence().joinToString("") { "$it: ${handshake.getFieldValue(it)}\r\n" }
            return listOf(ByteBuffer.wrap("HTTP/1.1 200 OK\r\n$head\r\n".encodeToByteArray() + handshake.content))
        }

        override fun copyInstance(): Draft = Pages()
    }

    private inner class Server(
        address: InetSocketAddress,
    ) : WebSocketServer(address, listOf(Pages(), Draft_6455())) {
        val started = CountDownLatch(1)
        var failure: Exception? = null
        private val links = ConcurrentHashMap<WebSocket, SocketLink>()

        init {
            isReuseAddr = true
        }

        override fun onStart() = started.countDown()

        // a page served: the connection closed once it is written
        override fun onOpen(
            socket: WebSocket,
            handshake: ClientHandshake,
        ) = if (socket.draft is Pages) {
            (socket as WebSocketImpl).flushAndClose(CloseFrame.NORMAL, "", false)
        } else {
            site.opened(SocketLink(socket).also { links[socket] = it })
        }

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
        code: String?,
    ) : WebSocketClient(uri) {
        val link: Link = SocketLink(this, outgoing = true, code = code)

        @Volatile var opened = false

        override fun onOpen(handshake: ServerHandshake) {
            opened = true
            site.opened(link)
        }

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

/** A page served over plain HTTP: its [content], of the media type [contentType] (as `text/html; charset=utf-8`) */
class StaticContent(
    val content: ByteArray,
    val contentType: String,
)
