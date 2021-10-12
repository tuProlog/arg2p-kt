package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.test.Test

class ArgumentGroundedLabellingTest {

    private fun prepareContext(solver: MutableSolver, graph: Graph) =
        arg2pScope {
            graph.arguments.forEach {
                solver.solve("context_assert"(it.toString()))
            }
            graph.attacks.forEach {
                solver.solve("context_assert"(it.toString()))
            }
            graph.supports.forEach {
                solver.solve("context_assert"(it.toString()))
            }
        }

    private fun checkResults(solver: MutableSolver, graph: Graph) =
        arg2pScope {
            graph.labellings.forEach {
                solver.solve("context_check"(it.toString()))
            }
        }

    private fun prepareGraph(): Graph {
        val arg1 = Argument(listOf("r5"), "r5", "[neg,s('Pippo')]")
        val arg2 = Argument(listOf("r4", "r3", "r1"), "r4", "[s('Pippo')]")
        val arg3 = Argument(listOf("r3", "r1"), "r3", "[r('Pippo')]")
        val arg4 = Argument(listOf("r2", "r0"), "r2", "[neg,r('Pippo')]")
        val arg5 = Argument(listOf("r1"), "r1", "[q('Pippo')]")
        val arg6 = Argument(listOf("r0"), "r0", "[a('Pippo')]")

        return Graph.of(
            listOf(
                LabelledArgument(arg1, "in"),
                LabelledArgument(arg2, "out"),
                LabelledArgument(arg3, "out"),
                LabelledArgument(arg4, "in"),
                LabelledArgument(arg5, "in"),
                LabelledArgument(arg6, "in")
            ),
            listOf(
                Attack(arg4, arg3),
                Attack(arg4, arg2),
                Attack(arg2, arg1),
                Attack(arg1, arg2)
            ),
            listOf(
                Support(arg3, arg2),
                Support(arg5, arg3),
                Support(arg6, arg4)
            )
        )
    }

    @Test
    fun labelArguments() {
        prepareGraph().also { graph ->
            arg2pScope {
                solver().also {
                    prepareContext(it, graph)
                    it.solve("grounded" call "argumentLabelling").first()
                    checkResults(it, graph)
                }
            }
        }
    }
}
