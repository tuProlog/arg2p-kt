package it.unibo.tuprolog.argumentation.federation

import kotlin.js.JsExport

/**
 * A connection between two sites over a medium the application provides, for sites to federate over it: what [send] sends is
 * to be given to [Federation.linkReceived] on the other side, in order, and [close] closes it, both sides told by
 * [Federation.linkClosed]. [outgoing] tells which side opened it, [code] the code it presents if so, and [address] where the
 * other side is, if known.
 */
@JsExport
class ExternalLink(
    private val sendFrame: (String) -> Unit,
    private val closeLink: () -> Unit,
    override val outgoing: Boolean = false,
    override val code: String? = null,
    override val address: String? = null,
) : Link {
    override fun send(frame: String) = sendFrame(frame)

    override fun close() = closeLink()
}
