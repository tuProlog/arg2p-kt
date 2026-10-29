package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.Arg2pException
import kotlin.js.Promise

/**
 * The links of [site] over WebRTC data channels, in browsers: to every site opening such links it is introduced to
 * (see [Site.onIntroduced]), what the two need to connect sent through the sites linked to both (see [Site.signal]).
 * Of two sites, the one with the lower identifier offers the link. [iceServers] are those of RTCPeerConnection
 * (none by default: the sites of a local network reach each other without them).
 *
 * Two sites with no site in common can be paired by hand instead, with no server: one [invite]s, the other [accept]s
 * the invitation, and the first [complete]s the pairing with the reply. The codes carry all the two need to connect
 * (their whole descriptions, the candidates included), compressed if the browser can. Each step may be given an
 * AbortSignal: once it aborts, the pairing is abandoned (its connection closed, with its link if it opened) and the
 * promise of the step, if still pending, rejected.
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

    // The connections paired by hand, by invitation, those waiting for their reply apart
    private val paired = mutableMapOf<String, dynamic>()
    private val invited = mutableMapOf<String, dynamic>()

    init {
        site.onIntroduced = { other -> if (site.id < other && other !in connections) handled { offer(other) } }
        site.onSignal = { from, payload -> handled { signalled(from, payload) } }
    }

    /** Closes every link */
    fun close() {
        (connections.values + paired.values + invited.values).toList().forEach { it.close() }
        connections.clear()
        paired.clear()
        invited.clear()
    }

    /** An invitation to pair with this site by hand, for another site to [accept]: abandoned once [signal] aborts, if given */
    fun invite(signal: dynamic = null): Promise<String> {
        if (aborted(signal)) return Promise.reject(abandoned())
        val invitation = "${site.id}#${pairings++}"
        val connection = peerConnection()
        invited[invitation] = connection
        val options: dynamic = js("({})")
        options.ordered = true
        attach(connection.createDataChannel("arg2p", options), null) {
            linked += invitation
            opening.remove(invitation)?.invoke()
        }
        val code: Promise<String> =
            connection
                .createOffer()
                .then { offer: dynamic -> connection.setLocalDescription(offer) }
                .then { _: dynamic -> gathered(connection) }
                .then { _: dynamic -> pack(invitation, connection.localDescription) }
        return abandonable(signal, code) { invitation }
    }

    /**
     * The reply to [invitation] (see [invite]), for the inviting site to [complete] the pairing: abandoned once [signal]
     * aborts, if given
     */
    fun accept(
        invitation: String,
        signal: dynamic = null,
    ): Promise<String> {
        var pairing: String? = null
        val answered =
            unpack(invitation).then { (id, offer) ->
                if (aborted(signal)) throw abandoned()
                pairing = id
                val connection = peerConnection()
                paired[id] = connection
                connection.ondatachannel = { event: dynamic -> attach(event.channel, null) { linked += id } }
                val reply: Promise<String> =
                    connection
                        .setRemoteDescription(offer)
                        .then { _: dynamic -> connection.createAnswer() }
                        .then { answer: dynamic -> connection.setLocalDescription(answer) }
                        .then { _: dynamic -> gathered(connection) }
                        .then { _: dynamic -> pack(id, connection.localDescription) }
                reply
            }
        return abandonable(signal, answered.unsafeCast<Promise<String>>()) { pairing }
    }

    /**
     * The pairing [invite]d completed with the [reply] of the other site: once the link is open; abandoned once [signal]
     * aborts, if given (as when the other site does not answer in time)
     */
    fun complete(
        reply: String,
        signal: dynamic = null,
    ): Promise<Unit> {
        var pairing: String? = null
        val completed =
            unpack(reply).then { (id, answer) ->
                if (aborted(signal)) throw abandoned()
                val connection = invited.remove(id) ?: throw IllegalStateException("No invitation $id")
                pairing = id
                paired[id] = connection
                Promise<Unit> { resolve, reject ->
                    opening[id] = { resolve(Unit) }
                    connection.setRemoteDescription(answer).catch { error: dynamic -> reject(error as Throwable) }
                }
            }
        return abandonable(signal, completed.unsafeCast<Promise<Unit>>()) { pairing }
    }

    // [promised], rejected once [signal] aborts (if given), the pairing then [pairing] (if known) abandoned unless its
    // link opened: its connection closed (its data channel with it), and nothing waiting for it to open
    private fun <T> abandonable(
        signal: dynamic,
        promised: Promise<T>,
        pairing: () -> String?,
    ): Promise<T> =
        if (signal == null) {
            promised
        } else {
            Promise { resolve, reject ->
                promised.then({ resolve(it) }, { reject(it) })
                val abandon = { _: dynamic ->
                    pairing()?.takeIf { it !in linked }?.let { id ->
                        opening.remove(id)
                        invited.remove(id)?.close()
                        paired.remove(id)?.close()
                    }
                    reject(abandoned())
                }
                if (aborted(signal)) {
                    abandon(null)
                } else {
                    val options: dynamic = js("({})")
                    options.once = true
                    signal.addEventListener("abort", abandon, options)
                }
            }
        }

    // The pairings invited so far, those whose opening someone waits for, and those whose link opened
    private var pairings = 0
    private val opening = mutableMapOf<String, () -> Unit>()
    private val linked = mutableSetOf<String>()

    private fun peerConnection(): dynamic {
        val configuration: dynamic = js("({})")
        configuration.iceServers = iceServers
        val constructor = js("RTCPeerConnection")
        return js("new constructor(configuration)")
    }

    private fun connection(other: String): dynamic {
        val connection = peerConnection()
        connections[other] = connection
        connection.onicecandidate = { event: dynamic ->
            if (event.candidate != null) site.signal(other, message("candidate", event.candidate))
        }
        return connection
    }

    // Once the candidates of [connection] are all gathered (or after a few seconds, with those found)
    private fun gathered(connection: dynamic): Promise<Unit> =
        Promise { resolve, _ ->
            if (connection.iceGatheringState == "complete") {
                resolve(Unit)
            } else {
                connection.onicegatheringstatechange = { _: dynamic ->
                    if (connection.iceGatheringState == "complete") resolve(Unit)
                }
                later(GATHERING) { resolve(Unit) }
            }
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

    // [channel] as a link of the site, to [other] if introduced (null if paired by hand), [opened] once it opens
    private fun attach(
        channel: dynamic,
        other: String?,
        opened: () -> Unit = {},
    ) {
        val link = ChannelLink(channel)
        val parts = StringBuilder()
        channel.onopen = { _: dynamic ->
            handled { site.opened(link) }
            opened()
        }
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
            if (other != null) {
                connections.remove(other)?.close()
                described -= other
            }
            handled { site.closed(link) }
        }
    }

    // A link on a data channel: each frame in chunks, all but the last marked as such
    private class ChannelLink(
        private val channel: dynamic,
    ) : Link {
        override fun send(frame: String) {
            if (channel.readyState != "open") return
            val chunks = chunked(frame, CHUNK)
            chunks.forEachIndexed { index, chunk -> channel.send("${if (index == chunks.lastIndex) LAST else MORE}$chunk") }
        }

        override fun close() {
            channel.close()
        }
    }

    companion object {
        /** The largest chunk of a frame sent in a message */
        const val CHUNK: Int = 16_000

        // Whether [signal], an AbortSignal if given, aborted
        private fun aborted(signal: dynamic): Boolean = signal != null && signal.aborted == true

        private fun abandoned() = Arg2pException("The pairing was abandoned")

        private const val MORE = '+'
        private const val LAST = '.'

        // The seconds the candidates of a pairing by hand are gathered at most
        private const val GATHERING = 5_000

        private fun message(
            key: String,
            value: dynamic,
        ): String {
            val message: dynamic = js("({})")
            message[key] = value
            return JSON.stringify(message)
        }

        // A code for a pairing by hand: the pairing and the description, as JSON, compressed in base64 if the browser
        // can ('z'), in base64 otherwise ('j')
        private fun pack(
            id: String,
            description: dynamic,
        ): Promise<String> {
            val content: dynamic = js("({})")
            content.id = id
            content.description = description
            val bytes = js("new TextEncoder()").encode(JSON.stringify(content))
            if (js("typeof CompressionStream") == "undefined") return Promise.resolve("j" + base64(bytes))
            return transformed(bytes, js("new CompressionStream('deflate-raw')")).then { compressed: dynamic -> "z" + base64(compressed) }
        }

        // The pairing and the description of a code made by pack
        private fun unpack(code: String): Promise<Pair<String, dynamic>> {
            val text = code.trim()
            val bytes = bytes(text.drop(1))
            val decoded =
                when (text.firstOrNull()) {
                    'z' -> transformed(bytes, js("new DecompressionStream('deflate-raw')"))
                    'j' -> Promise.resolve<dynamic>(bytes)
                    else -> return Promise.reject(IllegalArgumentException("Not a pairing code"))
                }
            return decoded.then { content: dynamic ->
                val parsed: dynamic = JSON.parse(js("new TextDecoder()").decode(content) as String)
                Pair<String, dynamic>(parsed.id as String, parsed.description)
            }
        }

        // [bytes] through [stream] (a CompressionStream or a DecompressionStream)
        private fun transformed(
            bytes: dynamic,
            stream: dynamic,
        ): Promise<dynamic> {
            val blob = js("new Blob([bytes])")
            val response = js("new Response(blob.stream().pipeThrough(stream))")
            val result: Promise<dynamic> = response.arrayBuffer().then { buffer: dynamic -> js("new Uint8Array(buffer)") }
            return result
        }

        private fun base64(bytes: dynamic): String {
            var binary = ""
            val length = bytes.length as Int
            for (index in 0 until length) binary += js("String.fromCharCode(bytes[index])") as String
            return js("btoa(binary)") as String
        }

        private fun bytes(base64: String): dynamic {
            val binary = js("atob(base64)") as String
            val bytes = js("new Uint8Array(binary.length)")
            for (index in binary.indices) bytes[index] = binary[index].code
            return bytes
        }
    }
}
