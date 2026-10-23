package it.unibo.tuprolog.argumentation.causality.modules

import it.unibo.tuprolog.argumentation.causality.causality
import it.unibo.tuprolog.argumentation.causality.isNessCause
import it.unibo.tuprolog.argumentation.causality.isNessOriginalCause
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

class CausalityModuleTest {
    private fun values(
        theory: String,
        goal: String,
    ) = Arg2pSolverFactory.causality(theory).let { solver ->
        solver
            .solve(Struct.parse(goal, solver.operators))
            .filter { it.isYes }
            .map { it.substitution.getByName("X").toString() }
            .toList()
    }

    @Test
    fun theProperSubsetsOfAList() {
        assertEquals(listOf("[a, b]", "[a, c]", "[a]", "[b, c]", "[b]", "[c]"), values("", "properSubsets([a, b, c], X)").sorted())
    }

    @Test
    fun theComplementOfAStatement() {
        assertEquals(listOf("'-'(a)"), values("", "compl(a, X)"))
        assertEquals(listOf("a"), values("", "compl(-a, X)"))
    }

    @Test
    fun theInterventionsRemovingACause() {
        // the complement of the cause, and the undercut of it as a rule
        assertEquals(listOf("['-'(a)]", "[undercut(a)]"), values("r1 :=> a.\nr2 : a => b.", "generateInterventions([], a, X)").take(2))
    }

    // ness/3 needs an argument for the cause IN in the grounded labelling: ness_original/3 does not
    @Test
    fun aNessCauseIsAccepted() {
        val solver = Arg2pSolverFactory.causality("tx2 : d => -r2.\nr1 :=> a.\nr2 : a => -b.")
        isNessCause(solver, "a", "-b")
        isNessCause(solver, "-d", "-b", false)
        isNessOriginalCause(solver, "-d", "-b")
    }
}
