package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.checkResults
import it.unibo.tuprolog.argumentation.core.TestingUtils.prepareContext
import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ArgumentLabellingTest {
    private fun prepareGraph(): Graph {
        val arg1 = Argument(listOf("r5"), "r5", "-s('Pippo')")
        val arg2 = Argument(listOf("r4", "r3", "r1"), "r4", "s('Pippo')")
        val arg3 = Argument(listOf("r3", "r1"), "r3", "r('Pippo')")
        val arg4 = Argument(listOf("r2", "r0"), "r2", "-r('Pippo')")
        val arg5 = Argument(listOf("r1"), "r1", "q('Pippo')")
        val arg6 = Argument(listOf("r0"), "r0", "a('Pippo')")

        return Graph.of(
            listOf(
                LabelledArgument(arg1, "in"),
                LabelledArgument(arg2, "out"),
                LabelledArgument(arg3, "out"),
                LabelledArgument(arg4, "in"),
                LabelledArgument(arg5, "in"),
                LabelledArgument(arg6, "in"),
            ),
            listOf(
                Attack(arg4, arg3),
                Attack(arg4, arg2),
                Attack(arg2, arg1),
                Attack(arg1, arg2),
            ),
            listOf(
                Support(arg3, arg2),
                Support(arg5, arg3),
                Support(arg6, arg4),
            ),
        )
    }

    private fun prepareCompleteGraph(): List<Graph> {
        val arg1 = Argument(listOf("r1"), "r1", "x")
        val arg2 = Argument(listOf("r2"), "r2", "-x")
        val arg3 = Argument(listOf("r3"), "r3", "y")
        val arg4 = Argument(listOf("r4"), "r4", "-y")

        return listOf(
            listOf(arg1 to "und", arg2 to "und", arg3 to "und", arg4 to "und"),
            listOf(arg1 to "und", arg2 to "und", arg3 to "out", arg4 to "in"),
            listOf(arg1 to "out", arg2 to "in", arg3 to "out", arg4 to "in"),
            listOf(arg1 to "in", arg2 to "out", arg3 to "out", arg4 to "in"),
            listOf(arg1 to "und", arg2 to "und", arg3 to "in", arg4 to "out"),
            listOf(arg1 to "out", arg2 to "in", arg3 to "in", arg4 to "out"),
            listOf(arg1 to "in", arg2 to "out", arg3 to "in", arg4 to "out"),
            listOf(arg1 to "out", arg2 to "in", arg3 to "und", arg4 to "und"),
            listOf(arg1 to "in", arg2 to "out", arg3 to "und", arg4 to "und"),
        ).map { res ->
            Graph.of(
                res.map { LabelledArgument(it.first, it.second) },
                listOf(
                    Attack(arg1, arg2),
                    Attack(arg2, arg1),
                    Attack(arg3, arg4),
                    Attack(arg4, arg3),
                ),
                emptyList(),
            )
        }
    }

    private fun preparePreferredGraph(): List<Graph> {
        val arg1 = Argument(listOf("r1"), "r1", "x")
        val arg2 = Argument(listOf("r2"), "r2", "-x")
        val arg3 = Argument(listOf("r3"), "r3", "y")
        val arg4 = Argument(listOf("r4"), "r4", "-y")

        return listOf(
            listOf(arg1 to "out", arg2 to "in", arg3 to "out", arg4 to "in"),
            listOf(arg1 to "in", arg2 to "out", arg3 to "out", arg4 to "in"),
            listOf(arg1 to "out", arg2 to "in", arg3 to "in", arg4 to "out"),
            listOf(arg1 to "in", arg2 to "out", arg3 to "in", arg4 to "out"),
        ).map { res ->
            Graph.of(
                res.map { LabelledArgument(it.first, it.second) },
                listOf(
                    Attack(arg1, arg2),
                    Attack(arg2, arg1),
                    Attack(arg3, arg4),
                    Attack(arg4, arg3),
                ),
                emptyList(),
            )
        }
    }

    private fun preparePreferred2(): List<Graph> {
        val arg1 = Argument(listOf("r1"), "r1", "a")
        val arg2 = Argument(listOf("r2"), "r2", "b")
        val arg3 = Argument(listOf("r3"), "r3", "c")
        val arg4 = Argument(listOf("r4"), "r4", "d")
        val arg5 = Argument(listOf("r5"), "r5", "e")

        return listOf(
            listOf(
                arg1 to "out",
                arg2 to "in",
                arg3 to "out",
                arg4 to "out",
                arg5 to "out",
            ),
            listOf(
                arg1 to "und",
                arg2 to "in",
                arg3 to "und",
                arg4 to "out",
                arg5 to "und",
            ),
        ).map { res ->
            Graph.of(
                res.map { LabelledArgument(it.first, it.second) },
                listOf(
                    Attack(arg1, arg4), // a → d
                    Attack(arg1, arg5), // a → e
                    Attack(arg2, arg4), // b → d
                    Attack(arg3, arg1), // c → a
                    Attack(arg4, arg1), // d → a
                    Attack(arg4, arg2), // d → b
                    Attack(arg4, arg5), // d → e
                    Attack(arg5, arg3), // e → c
                    Attack(arg5, arg4), // e → d
                ),
                emptyList(),
            )
        }
    }

    private fun prepareSemiStable(): List<Graph> {
        val arg1 = Argument(listOf("r1"), "r1", "a")
        val arg2 = Argument(listOf("r2"), "r2", "b")
        val arg3 = Argument(listOf("r3"), "r3", "c")
        val arg4 = Argument(listOf("r4"), "r4", "d")
        val arg5 = Argument(listOf("r5"), "r5", "e")

        return listOf(
            listOf(
                arg1 to "out",
                arg2 to "out",
                arg3 to "in",
                arg4 to "in",
                arg5 to "out",
            ),
        ).map { res ->
            Graph.of(
                res.map { LabelledArgument(it.first, it.second) },
                listOf(
                    Attack(arg1, arg4), // a → d
                    Attack(arg1, arg5), // a → e
                    Attack(arg2, arg4), // b → d
                    Attack(arg3, arg1), // c → a
                    Attack(arg4, arg1), // d → a
                    Attack(arg4, arg2), // d → b
                    Attack(arg4, arg5), // d → e
                    Attack(arg5, arg3), // e → c
                    Attack(arg5, arg4), // e → d
                ),
                emptyList(),
            )
        }
    }

    private fun prepareStable(): List<Graph> {
        val arg1 = Argument(listOf("r1"), "r1", "a")
        val arg2 = Argument(listOf("r2"), "r2", "b")
        val arg3 = Argument(listOf("r3"), "r3", "c")
        val arg4 = Argument(listOf("r4"), "r4", "d")
        val arg5 = Argument(listOf("r5"), "r5", "e")

        return listOf(
            Graph.of(
                emptyList(),
                listOf(
                    Attack(arg1, arg4), // a → d
                    Attack(arg1, arg5), // a → e
                    Attack(arg2, arg4), // b → d
                    Attack(arg3, arg1), // c → a
                    Attack(arg4, arg1), // d → a
                    Attack(arg4, arg2), // d → b
                    Attack(arg4, arg5), // d → e
                    Attack(arg5, arg3), // e → c
                    Attack(arg5, arg4), // e → d
                ),
                emptyList(),
            ),
        )
    }

    @Test
    fun labelArgumentsGrounded() {
        prepareGraph().also { graph ->
            arg2pScope {
                solver().also {
                    prepareContext(it, graph)
                    it.solve("grounded" call "argumentLabelling").first()
                    checkResults(it, graph.labellings)
                }
            }
        }
    }

    private fun checkSolutions(
        graph: List<Graph>,
        labeller: String,
    ) {
        val mapper = { labellings: List<LabelledArgument> -> labellings.map { l -> l.label + l.argument.topRule }.toSet() }
        val expected = graph.map { mapper(it.labellings) }
        graph.also { graph ->
            arg2pScope {
                solver().also { solver ->
                    prepareContext(solver, graph.first())
                    solver
                        .solve(labeller call "argumentLabelling")
                        .filter { res -> res.isYes }
                        .map { _ ->
                            println("ciao")
                            println(expected)
                            println(mapper(solver.graph().labellings))
                            assertTrue(expected.any { it == mapper(solver.graph().labellings) })
                        }.count()
                        .also { count ->
                            assertEquals(count, graph.size)
                        }
                }
            }
        }
    }

    @Test
    fun labelArgumentsComplete() = checkSolutions(prepareCompleteGraph(), "complete")

    @Test
    fun labelArgumentsPreferred() = checkSolutions(preparePreferredGraph(), "preferred")

    @Test
    fun labelArgumentsPreferredAgain() = checkSolutions(preparePreferred2(), "preferred")

    @Test
    fun labelArgumentsSemistable() = checkSolutions(prepareSemiStable(), "semistable")

    @Test
    fun labelArgumentsStable() = checkSolutions(prepareStable(), "stable")

    @Test
    fun labelArgumentsStableAgain() = checkSolutions(preparePreferredGraph(), "stable")
}
