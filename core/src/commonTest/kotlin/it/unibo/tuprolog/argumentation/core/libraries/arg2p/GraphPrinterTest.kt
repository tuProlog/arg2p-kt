package it.unibo.tuprolog.argumentation.core.libraries.arg2p

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.channel.OutputChannel
import kotlin.test.Test
import kotlin.test.assertTrue

// The graph and its labels written on the standard output (buildLabelSets/0)
class GraphPrinterTest {
    @Test
    fun theGraphAndItsLabelsArePrinted() {
        val printed = StringBuilder()
        val solver =
            Arg2pSolverFactory.solverOf(
                Arg2pSolver.default(listOf(Flags.none().library()), emptyList()),
                "f1 :=> a.\nr1 : a => b.\n",
                output = OutputChannel.of { printed.append(it) },
            )
        assertTrue(solver.solve(Struct.parse("buildLabelSets", solver.operators)).first().isYes)
        val text = printed.toString()
        assertTrue("HERE THE ARGUMENTATION GRAPH" in text, text)
        assertTrue("HERE THE STATEMENTS LABELLED IN" in text && "[b]" in text, text)
    }
}
