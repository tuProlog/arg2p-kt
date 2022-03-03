package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import kotlin.test.Test

class BurdenOfPersuasionLabellingTest {

    private fun prepareGraph(): Graph {
        val arg1 = Argument(listOf("r3", "r1", "r0"), "r1", "c")
        val arg2 = Argument(listOf("r3", "r0"), "r0", "a")
        val arg3 = Argument(listOf("r3"), "r3", "-c")
        val arg4 = Argument(listOf("r2"), "r2", "-a")

        return Graph(
            listOf(
                arg1,
                arg2,
                arg3,
                arg4
            ),
            listOf(
                Attack(arg1, arg1),
                Attack(arg4, arg2),
                Attack(arg4, arg1),
                Attack(arg3, arg1),
                Attack(arg2, arg4),
                Attack(arg1, arg3),
                Attack(arg1, arg2)
            ),
            listOf(
                Support(arg2, arg1),
                Support(arg3, arg2),
            )
        )
    }

    private fun groundedGraph(): Graph =
        prepareGraph().let {
            Graph.of(
                listOf(
                    LabelledArgument(it.arguments[0], "und"),
                    LabelledArgument(it.arguments[1], "und"),
                    LabelledArgument(it.arguments[2], "und"),
                    LabelledArgument(it.arguments[3], "und")
                ),
                it.attacks,
                it.supports
            )
        }

    private fun groundedBpGraph(): Graph =
        prepareGraph().let {
            Graph.of(
                listOf(
                    LabelledArgument(it.arguments[0], "und"),
                    LabelledArgument(it.arguments[1], "und"),
                    LabelledArgument(it.arguments[2], "und"),
                    LabelledArgument(it.arguments[3], "out")
                ),
                it.attacks,
                it.supports
            )
        }

    private fun groundedBpPartialGraph(): Graph =
        prepareGraph().let {
            Graph.of(
                listOf(
                    LabelledArgument(it.arguments[0], "und"),
                    LabelledArgument(it.arguments[1], "in"),
                    LabelledArgument(it.arguments[2], "und"),
                    LabelledArgument(it.arguments[3], "out")
                ),
                it.attacks,
                it.supports
            )
        }

    private fun groundedBpCompleteGraph(): Graph =
        prepareGraph().let {
            Graph.of(
                listOf(
                    LabelledArgument(it.arguments[0], "out"),
                    LabelledArgument(it.arguments[1], "in"),
                    LabelledArgument(it.arguments[2], "in"),
                    LabelledArgument(it.arguments[3], "out")
                ),
                it.attacks,
                it.supports
            )
        }

    @Test
    fun labelGroundedArguments() {
        groundedGraph().also { graph ->
            arg2pScope {
                TestingUtils.solver().also {
                    it.solve("context_assert"("abstractBp"(listOf("-a")))).first()
                    TestingUtils.prepareContext(it, graph)
                    it.solve("grounded" call "argumentLabelling").first()
                    TestingUtils.checkResults(it, graph.labellings)
                }
            }
        }
    }

    @Test
    fun labelBpGroundedArguments() {
        groundedBpGraph().also { graph ->
            arg2pScope {
                TestingUtils.solver().also {
                    it.solve("context_assert"("abstractBp"(listOf("-"("a"))))).first()
                    TestingUtils.prepareContext(it, graph)
                    it.solve("bp_grounded" call "argumentLabelling").first()
                    TestingUtils.checkResults(it, graph.labellings)
                }
            }
        }
    }

    @Test
    fun labelBpGroundedPartialArguments() {
        groundedBpPartialGraph().also { graph ->
            arg2pScope {
                TestingUtils.solver().also {
                    it.solve("context_assert"("abstractBp"(listOf("-"("a"))))).first()
                    TestingUtils.prepareContext(it, graph)
                    it.solve("bp_grounded_partial" call "argumentLabelling").first()
                    TestingUtils.checkResults(it, graph.labellings)
                }
            }
        }
    }

    @Test
    fun labelBpGroundedCompleteArguments() {
        groundedBpCompleteGraph().also { graph ->
            arg2pScope {
                TestingUtils.solver().also {
                    it.solve("context_assert"("abstractBp"(listOf("-"("a"))))).first()
                    TestingUtils.prepareContext(it, graph)
                    it.solve("bp_grounded_complete" call "argumentLabelling").first()
                    TestingUtils.checkResults(it, graph.labellings)
                }
            }
        }
    }
}
