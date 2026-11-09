package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import java.net.InetAddress
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import javax.net.ssl.SSLContext

/**
 * [parallel] federated with others, on any machine and engine, over WebSockets: it runs consumers of the graphs of the others,
 * and places the consumers of its graphs on them, in proportion to what each offers, if it [lends] (or else only the
 * latter). The federation closes with the parallel solver, or before it ([close]), the parallel solver going on alone.
 */
class Federation
    @JvmOverloads
    constructor(
        private val parallel: ThreadedSolver,
        lends: Boolean = true,
    ) : AutoCloseable {
        /** The site of the parallel solver, for what links it to others */
        @property:InternalArg2pApi
        val site: Site = Site(parallel.node, lends).apply { name = HOST_NAME }

        // made when the parallel solver first listens or connects
        private var links: WebSocketLinks? = null

        private fun links() = links ?: WebSocketLinks(site).also { links = it }

        init {
            parallel.node.onClose(::close)
        }

        /** The sites the parallel solver is federated with */
        val peers: Set<String> get() = site.peerSites

        /** The federation, as the parallel solver knows it: the sites */
        val stats: FederationStats get() = site.stats

        /** Called whenever a site joins or leaves the peers of the parallel solver, on the thread that saw it */
        var onPeersChanged: (() -> Unit)?
            get() = site.onPeersChanged
            set(value) {
                site.onPeersChanged = value
            }

        /** The name of the site of the parallel solver, as its peers are told: the name of its host by default */
        var name: String
            get() = site.name
            set(value) {
                site.name = value
            }

        /**
         * Whether the sites linking to this parallel solver must prove they know its [code], or the token of a pairing made with
         * it: refused otherwise, and after a few wrong codes from an address its codes are refused for a while
         */
        var requiresCode: Boolean
            get() = site.requiresCode
            set(value) {
                site.requiresCode = value
            }

        /**
         * The code the sites linking to this parallel solver prove they know, if it requires one: four digits, random, renewed
         * once a site pairs with the parallel solver
         */
        val code: String get() = site.code

        /** A new [code], the one before refused from now on (the pairings made stay) */
        fun newCode(): String = site.newCode()

        /**
         * Accepts the links of other sites (on any platform) over WebSockets, on [port] of [host] (an ephemeral port for
         * 0): the port taken
         */
        @JvmOverloads
        fun listen(
            port: Int = 0,
            host: String = "0.0.0.0",
        ): Int = links().listen(port, host)

        /**
         * Accepts the links of other sites over TLS too, on [port] of [host] (an ephemeral port for 0), the pages [serve]d
         * as well, for the browsers of other hosts to be a secure context: the port taken
         */
        @JvmOverloads
        fun listenSecurely(
            ssl: SSLContext,
            port: Int = 0,
            host: String = "0.0.0.0",
        ): Int = links().listenSecurely(ssl, port, host)

        /**
         * Federates the parallel solver with the site listening at [uri] (ws://host:port), once it admits this one: proving it knows
         * [code] if it requires one, the two paired then, and linked again to the same address with no code while they run.
         * An Arg2pException if it refuses the code, does not prove it knows it as well (a site claiming the identifier of
         * another), or does not answer in a few seconds.
         */
        @JvmOverloads
        fun connect(
            uri: String,
            code: String? = null,
        ) {
            val greeted = CompletableFuture<String?>()
            var opened: Link? = null
            try {
                links().connect(uri, WAITING, site.secretFor(uri, code)) { link ->
                    opened = link
                    site.linking(uri, link) { refusal -> greeted.complete(refusal) }
                }
                greeted.get(WAITING, TimeUnit.MILLISECONDS)?.let { throw Arg2pException("$uri refused the link: $it") }
            } catch (_: TimeoutException) {
                opened?.close()
                throw Arg2pException("$uri did not answer in time")
            } catch (failure: IllegalStateException) {
                throw Arg2pException("$uri cannot be reached: ${failure.message}")
            } finally {
                opened?.let { site.forgetGreeting(it) }
            }
        }

        /**
         * Serves [pages] over plain HTTP on the port the parallel solver listens on (see [WebSocketLinks.pages]): the page a GET
         * of a path is answered with, none (null) for a 404
         */
        fun serve(pages: (path: String) -> StaticContent?) {
            links().pages = pages
        }

        /** Closes the links to the other sites: the parallel solver goes on, alone */
        override fun close() {
            links?.close()
            links = null
        }

        companion object {
            // How long a link waits for the other site, in milliseconds
            private const val WAITING = 10_000L

            /** The name of this host, if it has one (empty otherwise): the name of the sites by default */
            @JvmStatic
            val HOST_NAME: String by lazy { runCatching { InetAddress.getLocalHost().hostName }.getOrDefault("") }
        }
    }
