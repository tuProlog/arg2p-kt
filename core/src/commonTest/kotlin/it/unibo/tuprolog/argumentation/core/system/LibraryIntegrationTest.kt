package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.bridge.BridgedArgument
import it.unibo.tuprolog.argumentation.core.bridge.BridgedAttack
import it.unibo.tuprolog.argumentation.core.bridge.BridgedGraph
import it.unibo.tuprolog.argumentation.core.bridge.BridgedSolution
import it.unibo.tuprolog.argumentation.core.bridge.JsBridge
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import kotlin.test.Test
import kotlin.test.assertEquals

class LibraryIntegrationTest {
    @Test
    fun libraryLoading() {
        val solver =
            ClassicSolverFactory.solverWithDefaultBuiltins(
                otherLibraries = Arg2pSolver.default().to2pLibraries(),
            )
        val query = Struct.parse("argTuProlog")
        val solutions = solver.solve(query)
        assertEquals(
            listOf(Solution.yes(query)),
            solutions.toList(),
        )
    }

    @Test
    fun library() {
        val libraries = Arg2pSolver.default().to2pLibraries()
        val theory = Arg2pSolver.default().to2pLibraries().clauses
        val libTheory = libraries.clauses
        assertEquals(libTheory, theory)
    }

    @Test
    fun jsBridge() {
        val theory =
            """
            d1 : bird(X) => flies(X).
            d2 : penguin(X) => bird(X).
            s1 : penguin(X) -> -flies(X).
            a1 :-> penguin(tweety).
            """.trimIndent()

        val flags =
            """
            graphBuildMode(standard_af).
            statementLabellingMode(statement).
            argumentLabellingMode(grounded_hash).
            orderingPrinciple(last).
            orderingComparator(elitist).
            graphExtension(standardPref).
            queryMode.
            """.trimIndent()

        val res = JsBridge.solve(queryText = "buildLabelSets", theoryText = theory, flagsText = flags, outputConsumer = { print(it) })
        assertEquals(
            res.i.next(),
            BridgedSolution(
                res = "yes",
                query = "buildLabelSets",
                substitutions = emptyArray(),
                exception = "",
                graph =
                    BridgedGraph(
                        arguments =
                            arrayOf(
                                BridgedArgument(id = "A0", descriptor = "A0 : a1 : penguin(tweety)", label = "in"),
                                BridgedArgument(id = "A1", descriptor = "A1 : A0,s1 : '-'(flies(tweety))", label = "in"),
                                BridgedArgument(id = "A2", descriptor = "A2 : A0,d2 : bird(tweety)", label = "in"),
                                BridgedArgument(id = "A3", descriptor = "A3 : A2,d1 : flies(tweety)", label = "out"),
                            ),
                        attacks = arrayOf(BridgedAttack(from = "A1", to = "A3")),
                    ),
            ),
        )
    }
}
