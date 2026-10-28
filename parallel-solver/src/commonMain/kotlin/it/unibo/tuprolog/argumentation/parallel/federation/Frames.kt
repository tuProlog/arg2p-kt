package it.unibo.tuprolog.argumentation.parallel.federation

import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter

/** What sites send one another on their links, as text (see [FrameCodec]) */
internal sealed interface Frame {
    /**
     * The first frame on a link, from each end: the site, its version (sites of different versions do not federate),
     * the workers it runs (null for no limit), whether it lends them to its peers, and whether it opens links of its
     * own to the sites it is introduced to (as browsers do, over WebRTC)
     */
    data class Hello(
        val site: String,
        val version: String,
        val capacity: Int?,
        val lends: Boolean,
        val introducible: Boolean = false,
    ) : Frame

    /** To a site opening links of its own: [site] does as well, and is reached through the sender of the frame */
    data class Introduce(
        val site: String,
    ) : Frame

    /** From [from] to [to], through the sites in between: what the two need to open a link of their own */
    data class Signal(
        val from: String,
        val to: String,
        val payload: String,
    ) : Frame

    /** The sites of the workers of a graph [session] builds, by their addresses: those of older sessions are dropped */
    data class Placement(
        val session: String,
        val workers: Map<String, String>,
    ) : Frame

    /** To a delegate: [site] left, the workers of its sessions are of no use any more */
    data class Forget(
        val site: String,
    ) : Frame

    /** To the site of [session]: a link its messages went through closed, some may be lost */
    data class Lost(
        val session: String,
    ) : Frame

    /** A message of [session] to the node [to], written by MessageCodec */
    data class Deliver(
        val session: String,
        val to: String,
        val message: String,
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
                            .text(if (frame.introducible) "1" else "0")
                    is Frame.Introduce -> text("Introduce").text(frame.site)
                    is Frame.Signal -> text("Signal").text(frame.from).text(frame.to).text(frame.payload)
                    is Frame.Placement ->
                        text("Placement").text(frame.session).list(frame.workers.entries.toList()) { text(it.key).text(it.value) }
                    is Frame.Forget -> text("Forget").text(frame.site)
                    is Frame.Lost -> text("Lost").text(frame.session)
                    is Frame.Deliver -> text("Deliver").text(frame.session).text(frame.to).text(frame.message)
                }
            }.toString()

    fun decode(text: String): Frame =
        TextReader(text).run {
            when (val kind = text()) {
                "Hello" -> Frame.Hello(text(), text(), number().takeIf { it >= 0 }, text() == "1", text() == "1")
                "Introduce" -> Frame.Introduce(text())
                "Signal" -> Frame.Signal(text(), text(), text())
                "Placement" -> Frame.Placement(text(), List(number()) { text() to text() }.toMap())
                "Forget" -> Frame.Forget(text())
                "Lost" -> Frame.Lost(text())
                "Deliver" -> Frame.Deliver(text(), text(), text())
                else -> error("Unknown frame $kind")
            }
        }
}
