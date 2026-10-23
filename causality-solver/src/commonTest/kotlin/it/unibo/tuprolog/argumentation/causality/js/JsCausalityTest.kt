package it.unibo.tuprolog.argumentation.causality.js

import it.unibo.tuprolog.argumentation.core.js.JsArg2p
import it.unibo.tuprolog.argumentation.core.js.JsSolverBuilder
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class JsCausalityTest {
    @Test
    fun aSolverWithTheCausalityModule() {
        val solver =
            JsSolverBuilder()
                .theory("r1 :=> a.\nr2 : a => b.")
                .module(JsCausality.module())
                .output { }
                .build()
        val solution = JsArg2p.solve(solver, "causality::ness(X, a, b)").next()
        assertTrue(solution.isYes)
        assertEquals("[- a]", solution.formatted("X"))
    }
}
