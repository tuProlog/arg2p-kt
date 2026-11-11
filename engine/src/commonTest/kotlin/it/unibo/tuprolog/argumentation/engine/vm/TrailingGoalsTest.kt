package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.time.Duration.Companion.seconds

/**
 * A call followed by goals that compile to no call (true, an inline unification, an inline type test) is not the last call of
 * its clause: the clause keeps its continuation in a frame, or the call would overwrite it (the clause returning to itself).
 */
class TrailingGoalsTest {
    private val theory =
        """
        c.
        c(1).
        t1 :- c, true.
        t2(Y) :- c, Y = 1.
        t3 :- c, atom(_).
        t4(X) :- c(X), true, true.
        t5 :- c, (true, true).
        """.trimIndent()

    private fun yes(goal: String): Boolean {
        val solver = solverWithTheory(theory)
        return solver.solve(Struct.parse(goal, solver.operators), 5.seconds).first().isYes
    }

    @Test
    fun aCallFollowedByTrue() {
        assertEquals(true, yes("t1"))
    }

    @Test
    fun aCallFollowedByAnInlineUnification() {
        assertEquals(true, yes("t2(1)"))
    }

    @Test
    fun aCallFollowedByAnInlineTypeTest() {
        assertEquals(false, yes("t3"))
    }

    @Test
    fun aCallFollowedByTrues() {
        assertEquals(true, yes("t4(1)"))
    }

    @Test
    fun aCallFollowedByAConjunctionOfTrues() {
        assertEquals(true, yes("t5"))
    }
}
