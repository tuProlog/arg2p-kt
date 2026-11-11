package it.unibo.tuprolog.argumentation.core.libraries.framework.context

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
        val solution = solver.solve(Struct.parse("contextReset, contextAssert(p(1)), $goal", solver.operators)).first()
        return when {
            solution.isYes -> "yes"
            solution.isHalt -> "error: " + solution.exception!!.message
            else -> "no"
        }
    }

    @Test
    fun checkingOutAMissingBranchLeavesTheActiveOne() {
        assertEquals("no", solve("contextCheckout(7)"))
        assertEquals("yes", solve("\\+ contextCheckout(7), contextActive(0), contextCheck(p(1))"))
        assertEquals("yes", solve("contextBranch(0, B), contextCheckout(0), contextDrop(B), \\+ contextCheckout(B), contextActive(0)"))
    }

    @Test
    fun aMissingBranchHasNoSolutions() {
        assertEquals("no", solve("contextCheck(7, p(_))"))
        assertEquals("no", solve("contextBranch(7, _)"))
    }

    @Test
    fun outputsAreUnified() {
        assertEquals("yes", solve("contextActive(0)"))
        assertEquals("no", solve("contextActive(3)"))
        assertEquals("yes", solve("contextBranch(0, 1), contextActive(1), contextLast(1)"))
        assertEquals("no", solve("contextBranch(0, 5)"))
        assertEquals("yes", solve("\\+ contextBranch(0, 5), contextActive(0), contextLast(0)"))
    }

    @Test
    fun branchesNamedByOtherTermsAreErrors() {
        assertTrue(solve("contextCheckout(_)").startsWith("error"))
        assertTrue(solve("contextCheckout(a)").startsWith("error"))
        assertTrue(solve("contextCheck(a, p(_))").startsWith("error"))
        assertTrue(solve("contextDropAfter(0, [a])").startsWith("error"))
    }

    // the copies of a term differing in the names of their variables share an identifier, which a term with an atom in
    // place of a variable does not
    @Test
    fun theIdentifiersOfTermsWithVariables() {
        assertEquals("yes", solve("contextId(p(X, Y), I), contextId(p(A, B), I), contextId(p(A, A), J), I \\== J"))
        assertEquals("yes", solve("contextId(p(X), I), contextId(p('_V0'), J), I \\== J"))
    }
}
