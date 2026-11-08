package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Arg2pException
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

// What the client solver is asked to evaluate, whatever it is: an error is an answer, and the parallel solver goes on
class ClientSolverRequestsTest {
    private val theory = "r1 :=> p(x).\nr2 : p(X) => q(X)."

    private fun parallel() = ThreadedSolver(threads = 2, parts = 2).also { it.load(theory) }

    @Test
    fun aStatementThatCannotBeReadIsAnError() {
        val parallel = parallel()
        assertFailsWith<Arg2pException> { parallel.solve("p(") }
        assertEquals(1, parallel.solve("q(x)").inArgs.size)
    }

    // the variables of the statement are its own, whatever their names
    @Test
    fun theVariablesOfAStatementAreItsOwn() {
        val parallel = parallel()
        listOf("p(A)", "p(L)", "q(S)").forEach { assertEquals(1, parallel.solve(it).inArgs.size, it) }
    }

    @Test
    fun aTheoryThatCannotBeReadIsAnError() {
        val parallel = ThreadedSolver(threads = 2, parts = 2).also { it.load("r1 : [] => .") }
        assertFailsWith<Arg2pException> { parallel.solve("a") }
        parallel.load(theory)
        assertEquals(1, parallel.solve("q(x)").inArgs.size)
    }

    // as the standard solver: ABA with a defeasible rule, ABA+ with no weak contraposition
    @Test
    fun aTheoryItsProfileRefusesIsAnError() {
        val defeasible = ThreadedSolver(threads = 2, parts = 2).also { it.load("graphBuildMode(aba).\nassumption(a).\nr : a => b.") }
        assertTrue("defeasible" in assertFailsWith<Arg2pException> { defeasible.solve("b") }.message.orEmpty())
        val noContraposition =
            ThreadedSolver(threads = 2, parts = 2).also {
                it.load(
                    "graphBuildMode(aba_plus).\nargumentLabellingMode(preferred).\nassumption(a0).\nassumption(a1).\n" +
                        "contrary(a0, x).\ncontrary(a1, y).\nr0 : a0, a1 -> x.\nsup(a0, a1).",
                )
            }
        assertTrue("weak contraposition" in assertFailsWith<Arg2pException> { noContraposition.solve("x") }.message.orEmpty())
    }

    // A Prolog check of a rule that raises an error while a consumer builds: the error of the evaluation, as the standard
    // solver raises it, not a parallel solver left waiting
    @Test
    fun anErrorOfAConsumerIsTheErrorOfTheEvaluation() {
        val parallel = ThreadedSolver(threads = 2, parts = 2).also { it.load("f1 :=> a.\nr1 : a, prolog(X is foo + 1) => b.\nf2 :=> c.") }
        assertFailsWith<Arg2pException> { parallel.solve("b") }
    }

    // A theory its profile refuses: the goals building its graph are refused, the other goals solved
    @Test
    fun aTheoryItsProfileRefusesSolvesTheOtherGoals() {
        val parallel = ThreadedSolver(threads = 2, parts = 2).also { it.load("graphBuildMode(aba).\nassumption(a).\nr : a => b.") }
        assertTrue(
            parallel.goals
                .solve("member(X, [1, 2])")
                .next()
                .isYes,
        )
        assertFailsWith<Arg2pException> { parallel.solve("b") }
    }
}
