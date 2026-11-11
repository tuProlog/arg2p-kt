package it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

// The acceptance of a statement (solve/1, /2, /4): the statements answering a goal, as answerMode states across the
// labellings
class StatementAcceptanceLibraryTest {
    // p and -p rebut each other: two preferred labellings, p IN in one, OUT in the other
    private val rebuttal = "f1 :=> p.\nf2 :=> -p.\nargumentLabellingMode(preferred).\n"

    private fun solutions(
        theory: String,
        goal: String,
    ): List<Solution> {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = Flags.defaults().library())
        return solver.solve(Struct.parse(goal, solver.operators)).filter { it.isYes }.toList()
    }

    private fun lists(solution: Solution) = listOf("I", "O", "U").joinToString(" ") { solution.substitution.getByName(it).toString() }

    @Test
    fun aGoalIsAnsweredOnceForEachLabelling() {
        assertEquals(setOf("[p] [] []", "[] [p] []"), solutions(rebuttal, "solve(p, I, O, U)").map { lists(it) }.toSet())
    }

    // credulous: IN where IN in some labelling; skeptical: OUT where OUT in some
    @Test
    fun theAnswerModesAcrossTheLabellings() {
        assertEquals(listOf("[p] [] []"), solutions(rebuttal + "answerMode(credulous).\n", "solve(p, I, O, U)").map { lists(it) })
        assertEquals(listOf("[] [p] []"), solutions(rebuttal + "answerMode(skeptical).\n", "solve(p, I, O, U)").map { lists(it) })
    }

    // -q(b) is not a statement of q(X)
    @Test
    fun aGoalWithVariablesIsAnsweredByEachStatement() {
        val theory = "f1 :=> q(a).\nf2 :=> q(b).\nf3 :=> -q(b).\nargumentLabellingMode(grounded).\n"
        assertEquals(listOf("[q(a)] [] [q(b)]"), solutions(theory, "solve(q(X), I, O, U)").map { lists(it) })
    }

    @Test
    fun aStatementIsAcceptedWhenIn() {
        val theory = "f1 :=> a.\nr1 : a => b.\nf2 :=> -b.\nsup(f2, r1).\ngraphExtension(standardPref).\n"
        assertTrue(solutions(theory, "solve(a)").isNotEmpty())
        assertFalse(solutions(theory, "solve(b)").isNotEmpty())
        assertEquals(listOf("[in(a)]"), solutions(theory, "solve(a, R)").map { it.substitution.getByName("R").toString() })
    }
}
