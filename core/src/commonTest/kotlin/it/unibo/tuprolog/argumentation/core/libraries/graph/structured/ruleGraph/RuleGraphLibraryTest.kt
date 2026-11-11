package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.ruleGraph

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.Support
import kotlin.test.Test

class RuleGraphLibraryTest {
    // the solver of the theory, its rules converted
    private fun solver() =
        solverWithTheory(
            """
                graphExtension(rebutRestriction).
                graphExtension(standardPref).
                orderingPrinciple(last).
                orderingComparator(democrat).
                r0 : -c => a.
                r1 : a => c.
                r2 : [] => -a.
                r3 : [] => -c.
            """,
        ).also { arg2pScope { it.solve("contextReset" and ("store" call "rulesRead")).first() } }

    private fun prepareGraph(): Graph {
        val arg1 =
            Argument(
                listOf("r3", "r1", "r0"),
                "r1",
                "c",
                groundings = listOf("a"),
                lastDefeasibleRules = listOf("r1"),
                defeasibleRules = listOf("r3", "r1", "r0"),
            )
        val arg2 =
            Argument(
                listOf("r3", "r0"),
                "r0",
                "a",
                groundings = listOf("-c"),
                lastDefeasibleRules = listOf("r0"),
                defeasibleRules = listOf("r3", "r0"),
            )
        val arg3 =
            Argument(
                listOf("r3"),
                "r3",
                "-c",
                lastDefeasibleRules = listOf("r3"),
                defeasibleRules = listOf("r3"),
            )
        val arg4 =
            Argument(
                listOf("r2"),
                "r2",
                "-a",
                lastDefeasibleRules = listOf("r2"),
                defeasibleRules = listOf("r2"),
            )

        return Graph(
            listOf(
                arg1,
                arg2,
                arg3,
                arg4,
            ),
            listOf(
                Attack(arg1, arg1, "rebut", arg3),
                Attack(arg4, arg2, "rebut", arg2),
                Attack(arg4, arg1, "rebut", arg2),
                Attack(arg3, arg1, "rebut", arg1),
                Attack(arg2, arg4, "rebut", arg4),
                Attack(arg1, arg3, "rebut", arg3),
                Attack(arg1, arg2, "rebut", arg3),
            ),
            listOf(
                Support(arg2, arg1),
                Support(arg3, arg2),
            ),
        )
    }

    @Test
    fun buildArgumentationGraph() {
        arg2pScope {
            solver().also {
                it.solve("ruleGraph" call "buildArgumentationGraph").first()
                TestingUtils.checkResults(it, prepareGraph())
            }
        }
    }

    @Test
    fun buildArguments() {
        arg2pScope {
            solver().also { solver ->
                solver.solve("ruleGraph" call "buildArguments").first()
                TestingUtils.checkResults(solver, prepareGraph().let { Graph(it.arguments, emptyList(), it.supports) })
            }
        }
    }
}
