package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

// The graphs of the labellings of a theory (Arg2pSolverFactory.evaluate): an error for a theory its profile refuses
class EvaluateTest {
    @Test
    fun theGraphOfEachLabelling() {
        val graphs = Arg2pSolverFactory.evaluate("r1 :=> a.\nr2 : a => b.", FlagsBuilder()).toList()
        assertEquals(1, graphs.size)
        assertEquals(2, graphs.single().labellings.size)
    }

    @Test
    fun aTheoryItsProfileRefusesIsAnError() {
        // a defeasible rule, which flat ABA does not have
        assertFailsWith<Arg2pException> { Arg2pSolverFactory.evaluate("r1 : [] => a.", FlagsBuilder(graphBuildMode = "aba")).toList() }
    }
}
