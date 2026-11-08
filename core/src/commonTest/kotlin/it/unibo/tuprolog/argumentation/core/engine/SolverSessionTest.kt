package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.engine.vm.Program
import it.unibo.tuprolog.argumentation.engine.vm.Record
import it.unibo.tuprolog.argumentation.engine.vm.SolverSession
import it.unibo.tuprolog.argumentation.engine.vm.VmSolver
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotSame
import kotlin.test.assertSame
import kotlin.test.assertTrue

/** The solvers of a session: one symbol table, so that a record of one is a record of the other as it is */
class SolverSessionTest {
    private fun solved(
        theory: String,
        goal: String,
        session: SolverSession?,
    ): Program {
        val solver = Arg2pPrologSolverFactory.solverWithDefaultBuiltins(staticKb = Theory.parse(theory)) as VmSolver
        if (session != null) SolverSession.join(solver, session)
        assertTrue(solver.solveOnce(Struct.parse(goal)).isYes, goal)
        return solver.currentProgram!!
    }

    @Test
    fun theSolversOfASessionShareTheirSymbols() {
        val session = SolverSession()
        val first = solved("p(alpha). q(X) :- p(X), atom_length(X, _).", "q(alpha)", session)
        val second = solved("r(beta, gamma(2.5)).", "r(beta, gamma(_))", session)
        val alone = solved("r(beta, gamma(2.5)).", "r(beta, _)", null)
        assertSame(first.symbols, second.symbols)
        assertNotSame(first.symbols, alone.symbols)
        // a term of one, as the other reads it
        val term = Term.parse("r(beta, gamma(2.5), [alpha, delta], 123456789012345678901234567890)")
        val record = Record.of(term, second)
        assertEquals(Record.of(term, first).key, record.key)
        assertEquals(term.toString(), Record.encoded(record, first).term.toString())
    }

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
