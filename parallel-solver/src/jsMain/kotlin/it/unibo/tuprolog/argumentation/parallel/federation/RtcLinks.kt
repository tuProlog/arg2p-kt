package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * The links of [site] over WebRTC data channels, in browsers: to every site opening such links it is introduced to
 * (see [Site.onIntroduced]), what the two need to connect sent through the sites linked to both (see [Site.signal]).
 * Of two sites, the one with the lower identifier offers the link. [iceServers] are those of RTCPeerConnection
 * (none by default: the sites of a local network reach each other without them).
 *
 * The frames are sent in chunks of at most [CHUNK] characters, as data channels bound the size of a message.
 */
class RtcLinks(
    private val site: Site,
    private val iceServers: Array<dynamic> = emptyArray(),
) {
    // The connections by site, and the candidates of each come before its description
    private val connections = mutableMapOf<String, dynamic>()
    private val described = mutableSetOf<String>()
    private val candidates = mutableMapOf<String, MutableList<dynamic>>()

    init {
        site.onIntroduced = { other -> if (site.id < other && other !in connections) handled { offer(other) } }
        site.onSignal = { from, payload -> handled { signalled(from, payload) } }
    }

    /** Closes every link */
    fun close() {
        connections.values.toList().forEach { it.close() }
        connections.clear()
    }

    private fun connection(other: String): dynamic {
        val configuration: dynamic = js("({})")
        configuration.iceServers = iceServers
        val constructor = js("RTCPeerConnection")
        val connection = js("new constructor(configuration)")
        connections[other] = connection
        connection.onicecandidate = { event: dynamic ->
            if (event.candidate != null) site.signal(other, message("candidate", event.candidate))
        }
        return connection
    }

    private fun offer(other: String) {
        val connection = connection(other)
        val options: dynamic = js("({})")
        options.ordered = true
        attach(connection.createDataChannel("arg2p", options), other)
        connection
            .createOffer()
            .then { offer: dynamic -> connection.setLocalDescription(offer) }
            .then { _: dynamic -> site.signal(other, message("description", connection.localDescription)) }
    }

    private fun signalled(
        from: String,
        payload: String,
    ) {
        val message: dynamic = JSON.parse(payload)
        val description = message.description
        when {
            description != null && description.type == "offer" -> {
                val connection = connections[from] ?: connection(from)
                connection.ondatachannel = { event: dynamic -> attach(event.channel, from) }
                connection
                    .setRemoteDescription(description)
                    .then { _: dynamic -> describedBy(from) }
                    .then { _: dynamic -> connection.createAnswer() }
                    .then { answer: dynamic -> connection.setLocalDescription(answer) }
                    .then { _: dynamic -> site.signal(from, message("description", connection.localDescription)) }
            }
            description != null ->
                connections[from]?.setRemoteDescription(description)?.then { _: dynamic -> describedBy(from) }
            message.candidate != null ->
                if (from in described) {
                    connections[from]?.addIceCandidate(message.candidate)
                } else {
                    candidates.getOrPut(from) { mutableListOf() }.add(message.candidate)
                }
        }
    }

    // The description of [other] set: the candidates come before it added
    private fun describedBy(other: String) {
        described += other
        candidates.remove(other)?.forEach { connections[other]?.addIceCandidate(it) }
    }

    private fun attach(
        channel: dynamic,
        other: String,
    ) {
        val link = ChannelLink(channel)
        val parts = StringBuilder()
        channel.onopen = { _: dynamic -> handled { site.opened(link) } }
        channel.onmessage = { event: dynamic ->
            val data = event.data as String
            parts.append(data, 1, data.length)
            if (data[0] == LAST) {
                val frame = parts.toString()
                parts.clear()
                handled { site.received(link, frame) }
            }
        }
        channel.onclose = { _: dynamic ->
            connections.remove(other)?.close()
            described -= other
            handled { site.closed(link) }
        }
    }

    // A link on a data channel: each frame in chunks, all but the last marked as such
    private class ChannelLink(
        private val channel: dynamic,
    ) : Link {
        override fun send(frame: String) {
            if (channel.readyState != "open") return
            val chunks = frame.chunked(CHUNK).ifEmpty { listOf("") }
            chunks.forEachIndexed { index, chunk -> channel.send("${if (index == chunks.lastIndex) LAST else MORE}$chunk") }
        }

        override fun close() {
            channel.close()
        }
    }

    companion object {
        /** The largest chunk of a frame sent in a message */
        const val CHUNK: Int = 16_000

        private const val MORE = '+'
        private const val LAST = '.'

        private fun message(
            key: String,
            value: dynamic,
        ): String {
            val message: dynamic = js("({})")
            message[key] = value
            return JSON.stringify(message)
        }
    }
}
