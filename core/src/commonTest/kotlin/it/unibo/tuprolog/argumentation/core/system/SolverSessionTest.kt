package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.engine.vm.program.SolverSession
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/** Solvers of Arg2P in a session: the results of solvers alone, and one identifier for a term of a context */
class SolverSessionTest {
    // Solvers of Arg2P in a session, whose modules the base of the session compiles: the results of solvers alone
    @Test
    fun theSolversOfASessionSolveAsTheOthers() {
        val theories =
            listOf(
                "f1 :=> a. f2 :=> -a. r1 : a => b. sup(f1, f2).",
                "f1 :=> p. r1 : p => q. r2 : q => -p. f3 :=> -q.",
            )
        val goal = "arg2p::solve(In, Out, Und)"
        val alone = theories.map { results(Arg2pSolverFactory.default(it), goal) }
        val session = SolverSession()
        repeat(2) {
            val shared = theories.map { results(Arg2pSolverFactory.default(it).also { s -> SolverSession.join(s, session) }, goal) }
            assertEquals(alone, shared)
        }
    }

    private fun results(
        solver: it.unibo.tuprolog.solve.Solver,
        goal: String,
    ): String =
        solver
            .solveOnce(Struct.parse(goal, solver.operators))
            .substitution.values
            .joinToString { it.toString() }

    // The identifiers of the contexts of a session: the same term the same identifier on every solver, kept by a reset
    @Test
    fun theContextsOfASessionShareTheirIdentifiers() {
        val session = SolverSession()
        val solvers = List(2) { Arg2pSolverFactory.default("f1 :=> a.").also { s -> SolverSession.join(s, session) } }

        fun id(
            solver: it.unibo.tuprolog.solve.Solver,
            goal: String,
        ) = solver
            .solveOnce(Struct.parse(goal, solver.operators))
            .substitution.values
            .single()
            .toString()
        val first = id(solvers[0], "context_id(argument(f(X, b)), Id)")
        id(solvers[1], "context_id(other(term), Id)")
        assertEquals(first, id(solvers[1], "context_id(argument(f(Y, b)), Id)"))
        assertEquals("b", id(solvers[1], "context_term($first, argument(f(_, B)))"))
        assertEquals(first, id(solvers[1], "context_reset, context_id(argument(f(Z, b)), Id)"))
    }
}
