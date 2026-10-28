package it.unibo.tuprolog.argumentation

import java.nio.file.Files
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** A sandboxed solver reads no file nor URL, and solves theories as any other */
class SandboxTest {
    // A module in a file of its own, and the goal calling it
    private val module = Files.createTempFile("arg2p-module", ".pl").also { Files.writeString(it, "hello(world).\n") }
    private val call = "module::call_module(['${module.toString().replace("\\", "/")}'], hello(X))"

    private fun solver(
        sandboxed: Boolean,
        theory: String = "",
    ) = Arg2p
        .builder()
        .theory(theory)
        .sandboxed(sandboxed)
        .build()

    @Test
    fun aSolverReadsTheModulesItIsGiven() {
        assertEquals("world", solver(false).solve(call).next().text("X"))
    }

    @Test
    fun aSandboxedSolverRefusesToReadThem() {
        val solution = solver(true).solve(call).next()
        assertTrue(solution.isHalt)
        assertTrue("Permission error" in solution.error.orEmpty(), solution.error)
    }

    @Test
    fun aSandboxedSolverSolvesTheoriesAsAnyOther() {
        val theory = "f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\nsup(f2, r1)."
        val sandboxed = solver(true, theory)
        assertEquals(listOf(Value.parse("-flies(tweety)")), sandboxed.answer("-flies(tweety)").inStatements)
        assertTrue(solver(true, "r1 :=> a.\nr2 : a => b.").causality.butFor("a", "b"))
    }
}
