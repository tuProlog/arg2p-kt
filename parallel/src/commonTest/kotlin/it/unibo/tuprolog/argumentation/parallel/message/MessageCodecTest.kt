package it.unibo.tuprolog.argumentation.parallel.message

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class MessageCodecTest {
    // texts with the characters of the encoding itself, digits and colons, and lines
    private val text = "12:ab: 'x:y'(3).\nnext"

    @Test
    fun everyMessageIsDecodedAsItWasEncoded() {
        val messages =
            listOf(
                Reset,
                Rebuild,
                Stop,
                Configure("queryMode.\n"),
                Load(text),
                Add(text),
                RequireEvaluation(text, "client_1"),
                RequireSolution("run_1", text, "client_2"),
                RequireSolution("run_1", null, "client_3"),
                RequireAnswer(text, "client_4"),
            )
        messages.forEach { assertEquals(it, MessageCodec.decode(MessageCodec.encode(it))) }
    }

    // what is not a message is refused before anything is made of it, a list of a billion elements in a few characters
    // (as one exhausting the memory) as well
    @Test
    fun whatIsNoMessageIsRefused() {
        listOf("", "Reset", "x:Reset", "-1:", "3:Add", "9:Unknown", "15:RequireSolution5:run_110:1000000000")
            .forEach { assertFailsWith<IllegalArgumentException>(it) { MessageCodec.decode(it) } }
    }
}
