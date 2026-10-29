package it.unibo.tuprolog.argumentation.lsp

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.time.Duration.Companion.milliseconds

/** The time limits of the works of the servers */
class DeadlineTest {
    // A work that waits until it is interrupted, and then ends as if it found nothing (as the settings of a solver
    // stopped read as none)
    private fun swallowing(): String =
        try {
            Thread.sleep(60_000)
            "found"
        } catch (_: InterruptedException) {
            "nothing"
        }

    @Test
    fun aWorkOutOfTimeFailsAsSuchEvenIfItEnds() {
        val failure = assertFailsWith<TimedOut> { Deadline.timed(200.milliseconds) { swallowing() } }
        assertEquals(TimedOut(200.milliseconds).message, failure.message)
        // the interruption of the deadline is over with it
        assertFalse(Thread.currentThread().isInterrupted)
    }

    @Test
    fun aWorkOutOfTimeAnswersWithWhatItFoundIfItSaysSo() {
        assertEquals("nothing", Deadline.timed(200.milliseconds, { it == "nothing" }) { swallowing() })
    }

    @Test
    fun aWorkInTimeAnswers() {
        assertEquals("found", Deadline.timed(60_000.milliseconds) { "found" })
    }

    @Test
    fun aWorkOutOfTimeThatFailsFailsAsOutOfTime() {
        assertFailsWith<TimedOut> { Deadline.timed(200.milliseconds) { Thread.sleep(60_000) } }
    }
}
