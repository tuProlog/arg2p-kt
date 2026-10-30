package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The axioms of time (temporal), asked in query mode
class TemporalModuleTest {
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

    // The label of each statement: in, out, und, or none when no argument concludes it
    private fun labels(
        theory: String,
        vararg statements: String,
    ): List<String> =
        statements.map { statement ->
            val solver = Arg2pSolverFactory.default(theory = "temporal.\n$theory", settings = FlagsBuilder().create())
            val solution = solver.solve(Struct.parse("answerQuery($statement, I, O, U)", solver.operators)).first()
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
            listOf("none", "in", "out", "in", "out"),
            labels(interrupted, "holdsAt(f, 1)", "holdsAt(f, 5)", "holdsAt(f, 7)", "holdsAt(f, 9)", "holdsAt(f, from(1))"),
        )
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

    // h holds over (0, 6], so over (0, 4] as well: a region holds for each of its times, it is no period of its own
    @Test
    fun aDefeatedEndEndsNoPeriod() {
        assertEquals(
            listOf("out", "in", "in", "in", "out"),
            labels(
                defeatedEnd,
                "ended(h, 4)",
                "holdsAt(h, between(0, 6))",
                "holdsAt(h, between(0, 4))",
                "holdsAt(h, 5)",
                "holdsAt(h, 7)",
            ),
        )
    }
}
