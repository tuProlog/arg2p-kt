package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.TestingUtils.testYesGoal
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test

class StatementLabellingTest {

    private fun prepareGraph(): List<LabelledArgument> =
        listOf(
            LabelledArgument(Argument(listOf("r3", "r1", "r0"), "r1", "[c]"), "out"),
            LabelledArgument(Argument(listOf("r3", "r0"), "r0", "[a]"), "in"),
            LabelledArgument(Argument(listOf("r3"), "r3", "[neg,c]"), "in"),
            LabelledArgument(Argument(listOf("r2"), "r2", "[neg,a]"), "out")
        )

    @Test
    fun labelStatements() {
        prepareGraph().also { arguments ->
            arg2pScope {
                TestingUtils.solver().also { solver ->
                    arguments.forEach {
                        solver.solve("context_assert"(it.toTerm())).first()
                    }
                    solver.solve("statement" call "statementLabelling").first()
                    arguments.forEach {
                        testYesGoal(
                            "context_check"(
                                when (it.label) {
                                    "in" -> "statIn"(Struct.parse(it.argument.conclusion))
                                    "out" -> "statOut"(Struct.parse(it.argument.conclusion))
                                    else -> "statUnd"(Struct.parse(it.argument.conclusion))
                                }
                            ),
                            solver
                        )
                    }
                }
            }
        }
    }
}
