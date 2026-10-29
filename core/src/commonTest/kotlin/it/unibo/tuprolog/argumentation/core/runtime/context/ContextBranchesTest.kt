package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The branches of the context named by the primitives: a branch that does not exist (never created, or dropped) is
 * not made active and has no solutions, a branch named by something else than an integer is an error, and the
 * outputs are unified rather than bound.
 */
class ContextBranchesTest {
    private fun solve(goal: String): String {
        val solver = solverWithTheory("")
        val solution = solver.solve(Struct.parse("context_reset, context_assert(p(1)), $goal", solver.operators)).first()
        return when {
            solution.isYes -> "yes"
            solution.isHalt -> "error: " + solution.exception!!.message
            else -> "no"
        }
    }

    @Test
    fun checkingOutAMissingBranchLeavesTheActiveOne() {
        assertEquals("no", solve("context_checkout(7)"))
        assertEquals("yes", solve("\\+ context_checkout(7), context_active(0), context_check(p(1))"))
        assertEquals("yes", solve("context_branch(0, B), context_checkout(0), context_drop(B), \\+ context_checkout(B), context_active(0)"))
    }

    @Test
    fun aMissingBranchHasNoSolutions() {
        assertEquals("no", solve("context_check(7, p(_))"))
        assertEquals("no", solve("context_all(7, p(_), _)"))
        assertEquals("yes", solve("context_absent(7, p(_))"))
        assertEquals("no", solve("context_branch(7, _)"))
    }

    @Test
    fun outputsAreUnified() {
        assertEquals("yes", solve("context_active(0)"))
        assertEquals("no", solve("context_active(3)"))
        assertEquals("yes", solve("context_branch(0, 1), context_active(1), context_last(1)"))
        assertEquals("no", solve("context_branch(0, 5)"))
        assertEquals("yes", solve("\\+ context_branch(0, 5), context_active(0), context_last(0)"))
    }

    @Test
    fun branchesNamedByOtherTermsAreErrors() {
        assertTrue(solve("context_checkout(_)").startsWith("error"))
        assertTrue(solve("context_checkout(a)").startsWith("error"))
        assertTrue(solve("context_check(a, p(_))").startsWith("error"))
        assertTrue(solve("context_drop_after(0, [a])").startsWith("error"))
    }

    // the copies of a term differing in the names of their variables share an identifier, which a term with an atom in
    // place of a variable does not
    @Test
    fun theIdentifiersOfTermsWithVariables() {
        assertEquals("yes", solve("context_id(p(X, Y), I), context_id(p(A, B), I), context_id(p(A, A), J), I \\== J"))
        assertEquals("yes", solve("context_id(p(X), I), context_id(p('_V0'), J), I \\== J"))
    }
}
