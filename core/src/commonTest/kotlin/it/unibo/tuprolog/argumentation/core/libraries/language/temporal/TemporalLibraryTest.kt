package it.unibo.tuprolog.argumentation.core.libraries.language.temporal

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The axioms of time (temporal), asked in query mode, and on the whole graph
class TemporalLibraryTest {
    // f starts at 1, and the theory states it does not hold at 7
    private val interrupted = "f1 :=> happens(e1, 1).\nf2 :=> initiates(e1, f).\nc1 :=> -holdsAt(f, 7).\n"

    // g ends at 5, with no start known
    private val backward = "f3 :=> happens(e2, 5).\nf4 :=> terminates(e2, g).\n"

    // h starts at 0 and ends at 4 and 6, the end at 4 defeated
    private val defeatedEnd =
        "f5 :=> happens(e0, 0).\nf6 :=> initiates(e0, h).\nf7 :=> happens(e3, 4).\nf8 :=> terminates(e3, h).\n" +
            "f9 :=> happens(e4, 6).\nf10 :=> terminates(e4, h).\nu1 :=> -t_stop(e3, h, 4).\n"

    // alive starts at 0, dead at 3, the two declared in conflict
    private val declared =
        "f11 :=> happens(birth, 0).\nf12 :=> initiates(birth, alive).\nf13 :=> happens(shot, 3).\n" +
            "f14 :=> initiates(shot, dead).\nconflict([dead], [alive]).\n"

    // f started twice (at 1, and again at 3 while it holds), the second start derived in a later round
    private val restarted =
        "f15 :=> happens(e1, 1).\nf16 :=> initiates(e1, f).\nf17 :=> happens(e2, 3).\nf18 :=> x.\n" +
            "r1 : x => initiates(e2, f).\n"

    // The label of each statement, the same in query mode and on the whole graph: in, out, und, or none when no
    // argument concludes it
    private fun labels(
        theory: String,
        vararg statements: String,
    ): List<String> {
        val (query, whole) = listOf(true, false).map { labels(theory, it, *statements) }
        assertEquals(whole, query, "query mode against the whole graph")
        return query
    }

    private fun labels(
        theory: String,
        queryMode: Boolean,
        vararg statements: String,
    ): List<String> =
        statements.map { statement ->
            val solver =
                Arg2pSolverFactory.default(theory = "temporal.\n$theory", settings = Flags.defaults().queryMode(queryMode).library())
            val solution = solver.solve(Struct.parse("solve($statement, I, O, U)", solver.operators)).first()
            val term = Struct.parse(statement, solver.operators)
            listOf("I" to "in", "O" to "out", "U" to "und")
                .firstOrNull { (name, _) ->
                    solution.substitution
                        .getByName(name)!!
                        .castToList()
                        .toList()
                        .any { it == term }
                }?.second ?: "none"
        }

    @Test
    fun aStatedTimeInterruptsAPeriodThere() {
        assertEquals(
            listOf("none", "in", "out", "in"),
            labels(interrupted, "holdsAt(f, 1)", "holdsAt(f, 5)", "holdsAt(f, 7)", "holdsAt(f, 9)"),
        )
    }

    // the times no rule asks for are answered by the pieces of the period around the stated time: dense times too
    @Test
    fun anyTimeIsAnsweredByThePieceHoldingIt() {
        assertEquals(
            listOf("in", "in", "out", "in"),
            labels(interrupted, "holdsAt(f, 100)", "holdsAt(f, 6.5)", "holdsAt(f, 7)", "holdsAt(f, 7.5)"),
        )
    }

    // f over (1, 9], -f stated at 5: the period is interrupted at 5 alone; after 9 the period after the start is closed
    // (out: the argument for every time after 1 stands for 10 as well, defeated)
    @Test
    fun aStatedTimeInterruptsABoundedPeriod() {
        val bounded =
            interrupted.replace("c1 :=> -holdsAt(f, 7).\n", "c1 :=> -holdsAt(f, 5).\n") +
                "f19 :=> happens(e5, 9).\nf20 :=> terminates(e5, f).\n"
        assertEquals(
            listOf("in", "out", "in", "in", "out"),
            labels(bounded, "holdsAt(f, 4)", "holdsAt(f, 5)", "holdsAt(f, 6)", "holdsAt(f, 9)", "holdsAt(f, 10)"),
        )
    }

    // an open query lists the periods, each with the times it holds for
    @Test
    fun anOpenQueryListsThePeriods() {
        val solver = Arg2pSolverFactory.default(theory = "temporal.\n$interrupted", settings = Flags.defaults().library())
        val solution = solver.solve(Struct.parse("solve(holdsAt(f, T), I, O, U)", solver.operators)).first()
        val accepted =
            solution.substitution
                .getByName("I")!!
                .castToList()
                .toList()
        // the pieces before and after the interruption at 7
        assertEquals(2, accepted.count { it.toString().contains("7") }, accepted.toString())
    }

    @Test
    fun anEndHoldsBackwardsToItsStart() {
        assertEquals(listOf("in", "in", "none"), labels(backward, "holdsAt(g, 3)", "holdsAt(g, 5)", "holdsAt(g, 6)"))
    }

    // a start ends what the conflict model sets against it, a declared conflict as a negation
    @Test
    fun aStartEndsWhatConflictsWithIt() {
        assertEquals(
            listOf("in", "in", "out", "in"),
            labels(declared, "ended(alive, 3)", "holdsAt(alive, 2)", "holdsAt(alive, 5)", "holdsAt(dead, 5)"),
        )
    }

    // h holds over (0, 6]: the period (0, 4] is not one, its end defeated, and 4 is a time of the longer one
    @Test
    fun aDefeatedEndEndsNoPeriod() {
        assertEquals(
            listOf("out", "in", "in", "in", "out"),
            labels(
                defeatedEnd,
                "ended(h, 4)",
                "holdsAt(h, 6)",
                "holdsAt(h, 4)",
                "holdsAt(h, 5)",
                "holdsAt(h, 7)",
            ),
        )
    }

    // The whole graph has the periods of the starts derived in later rounds: no axiom leaves a check to a binding a
    // query would give (t_still binds the start it is about)
    @Test
    fun theWholeGraphHasThePeriods() {
        val solver = Arg2pSolverFactory.default(theory = "temporal.\n$restarted", settings = Flags.defaults().queryMode(false).library())
        val solution = solver.solve(Struct.parse("solve(I, O, U)", solver.operators)).first()
        assertTrue(solution.isYes, solution.toString())
        val statements =
            solution.substitution
                .getByName("I")!!
                .castToList()
                .toList()
                .map { it.toString() }
        assertTrue(statements.any { it.startsWith("[holdsAt(f, ") }, statements.toString())
        // the second start is no new start (t_already, which t_still would undercut after an end)
        assertTrue("[started(f, 1)]" in statements && statements.any { "t_start(e2, f, 3)" in it }, statements.toString())
    }
}
