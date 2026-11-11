package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.Arg2pPrologSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.exception.LogicError
import it.unibo.tuprolog.solve.exception.Warning
import it.unibo.tuprolog.solve.exception.warning.MissingPredicate
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs
import kotlin.test.assertTrue

// A procedure that neither the clauses nor the libraries define, as the flag unknown says (ISO, as tuProlog's solver): an
// existence error, a warning and a failure, or a failure; a procedure with no clause left is defined, and fails
class UnknownFlagTest {
    private val warnings = ArrayList<Warning>()

    private fun solve(
        unknown: it.unibo.tuprolog.core.Term?,
        goal: String,
    ): Solution {
        val flags = if (unknown == null) FlagStore.DEFAULT else FlagStore.DEFAULT.set(Unknown, unknown)
        val solver =
            Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
                flags = flags,
                staticKb = Theory.parse("known(1)."),
                warnings = OutputChannel.of { warnings += it },
            )
        return solver.solveOnce(Struct.parse(goal, solver.operators))
    }

    @Test
    fun anUnknownProcedureIsAnErrorWithError() {
        val solution = solve(Unknown.ERROR, "missing(1)")
        assertIs<Solution.Halt>(solution)
        // (the second argument of the error, its context, is the implementation's)
        assertEquals(Struct.parse("existence_error(procedure, '/'(missing, 1))"), assertIs<LogicError>(solution.exception).errorStruct[0])
        // caught as any other error, and the same through call/1
        val caught = solve(Unknown.ERROR, "catch(call(missing), error(existence_error(procedure, PI), _), true)")
        assertEquals(Struct.parse("'/'(missing, 0)"), caught.substitution.getByName("PI"))
    }

    @Test
    fun anUnknownProcedureWarnsAndFailsWithWarning() {
        assertTrue(solve(Unknown.WARNING, "missing(1)").isNo)
        assertEquals(listOf("missing/1"), warnings.map { (it as MissingPredicate).signature.let { s -> "${s.name}/${s.arity}" } })
    }

    @Test
    fun anUnknownProcedureFailsWithFail() {
        assertTrue(solve(Unknown.FAIL, "missing(1)").isNo)
        assertEquals(emptyList(), warnings)
    }

    // (tuProlog's default: a warning)
    @Test
    fun theDefaultIsTheFlagOfTheSolver() {
        assertTrue(solve(null, "missing").isNo)
        assertEquals(1, warnings.size)
    }

    @Test
    fun aProcedureWithNoClauseLeftIsDefined() {
        assertTrue(solve(Unknown.ERROR, "known(X)").isYes)
        assertTrue(solve(Unknown.ERROR, "assertz(gone(1)), retract(gone(1)), \\+ gone(_)").isYes)
    }
}
