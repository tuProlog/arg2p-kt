package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter

/** What sites send one another on their links, as text (see [FrameCodec]) */
internal sealed interface Frame {
    /**
     * The greeting of a site, on each link: the site, its version (sites of different versions do not federate), the
     * consumers it offers (null for no limit), whether it runs those of its peers, and its [name]. The first one on a link
     * carries as well the [nonce]
     * the other site proves its secret on (a code, or the token of a pairing), the [proof] of the secret of this one on
     * the nonce of the other (see [Pairing.proof]), and, from a site paired just now by its code, the [token] of the
     * pairing. Neither a code nor a token is ever sent but in that last case.
     */
    data class Hello(
        val site: String,
        val version: String,
        val capacity: Int?,
        val lends: Boolean,
        val nonce: String? = null,
        val proof: String? = null,
        val token: String? = null,
        val name: String = "",
    ) : Frame

    /**
     * The first frame on a link from the site that did not open it (see [Link.outgoing]): the [nonce] the other proves
     * its code or token on, greeting it then
     */
    data class Challenge(
        val nonce: String,
    ) : Frame

    /** To the site that opened a link: the link is refused (the code it presented is not that of this site), why */
    data class Refused(
        val reason: String,
    ) : Frame

    /** What goes between the consumers of a graph and its store (see HostFrame), as text */
    data class Host(
        val frame: String,
    ) : Frame
}

/** The frames as text: their fields as MessageCodec writes those of the messages */
internal object FrameCodec {
    fun encode(frame: Frame): String =
        TextWriter()
            .apply {
                when (frame) {
                    is Frame.Hello ->
                        text("Hello")
                            .text(frame.site)
                            .text(frame.version)
                            .number(frame.capacity ?: -1)
                            .text(if (frame.lends) "1" else "0")
                            .texts(listOfNotNull(frame.nonce))
                            .texts(listOfNotNull(frame.proof))
                            .texts(listOfNotNull(frame.token))
                            .text(frame.name)
                    is Frame.Challenge -> text("Challenge").text(frame.nonce)
                    is Frame.Refused -> text("Refused").text(frame.reason)
                    is Frame.Host -> text("Host").text(frame.frame)
                }
            }.toString()

    /** The frame of [text]: an IllegalArgumentException if it is none */
    fun decode(text: String): Frame =
        TextReader(text).run {
            when (val kind = text()) {
                "Hello" ->
                    Frame.Hello(
                        text(),
                        text(),
                        number().takeIf { it >= 0 },
                        text() == "1",
                        optional(),
                        optional(),
                        optional(),
                        text(),
                    )
                "Challenge" -> Frame.Challenge(text())
                "Refused" -> Frame.Refused(text())
                "Host" -> Frame.Host(text())
                else -> throw IllegalArgumentException("Unknown frame $kind")
            }
        }

    // A field written as a list of none or one text
    private fun TextReader.optional(): String? = texts().singleOrNull()
}

/**
 * The longest frame a site receives, in bytes: a longer one closes its link, rather than filling the memory of the
 * site (see the transports)
 */
internal const val LONGEST_FRAME: Int = 64 * 1024 * 1024

/**
 * The chunks of [frame], of at most [size] characters, one at least, for the transports bounding their messages: none
 * ends between the two halves of a character beyond 16 bits (a surrogate pair), which a transport sending UTF-8 would
 * not carry
 */
internal fun chunked(
    frame: String,
    size: Int,
): List<String> {
    val chunks = mutableListOf<String>()
    var start = 0
    while (start < frame.length) {
        var end = minOf(start + size, frame.length)
        if (end < frame.length && end - 1 > start && frame[end - 1].isHighSurrogate()) end--
        chunks += frame.substring(start, end)
        start = end
    }
    return chunks.ifEmpty { listOf("") }
}
