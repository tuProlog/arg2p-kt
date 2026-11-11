package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.engine.solver.VmSolver
import it.unibo.tuprolog.argumentation.engine.store.Record
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.vm.program.SolverSession
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
}
