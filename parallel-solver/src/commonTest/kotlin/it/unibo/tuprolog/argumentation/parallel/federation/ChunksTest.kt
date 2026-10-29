package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The chunks of a frame, for a transport bounding its messages
class ChunksTest {
    @Test
    fun aChunkEndsBetweenCharacters() {
        // a character beyond 16 bits, two halves, where a chunk would end
        val frame = "ab\uD83D\uDE00c".repeat(5)
        listOf(2, 3, 4, 7, 100).forEach { size ->
            val chunks = chunked(frame, size)
            assertEquals(frame, chunks.joinToString(""))
            assertTrue(chunks.all { it.length <= size && !it.last().isHighSurrogate() }, "$size: $chunks")
        }
        assertEquals(listOf(""), chunked("", 3))
    }
}
