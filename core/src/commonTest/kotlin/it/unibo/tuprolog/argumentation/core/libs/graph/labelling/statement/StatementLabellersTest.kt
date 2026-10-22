package it.unibo.tuprolog.argumentation.core.libs.graph.labelling.statement

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class StatementLabellersTest {
    // a has an argument IN and one OUT, b one UND, c one OUT
    private fun labelled(labeller: String): Map<String, Set<String>> {
        val module = ModuleTesting()
        module.run(
            "context_assert(in([[r1], r1, [a], [], [[r1], [r1], []]]))",
            "context_assert(out([[r2], r2, [a], [], [[r2], [r2], []]]))",
            "context_assert(und([[r3], r3, [b], [], [[r3], [r3], []]]))",
            "context_assert(out([[r4], r4, [c], [], [[r4], [r4], []]]))",
            "$labeller::statementLabelling",
        )
        return listOf("statIn", "statOut", "statUnd").associateWith { label ->
            module.values("context_check($label([S]))", "S").toSet()
        }
    }

    @Test
    fun aStatementTakesTheBestLabelOfItsArguments() {
        assertEquals(
            mapOf("statIn" to setOf("a"), "statOut" to setOf("c"), "statUnd" to setOf("b")),
            labelled("statement"),
        )
    }

    @Test
    fun passingThroughAStatementTakesTheLabelsOfAllItsArguments() {
        assertEquals(
            mapOf("statIn" to setOf("a"), "statOut" to setOf("a", "c"), "statUnd" to setOf("b")),
            labelled("statement_pass_through"),
        )
    }

    @Test
    fun theBinaryLabellingTellsInFromTheRest() {
        assertEquals(
            mapOf("statIn" to setOf("a"), "statOut" to setOf("a", "b", "c"), "statUnd" to emptySet()),
            labelled("statement_binary"),
        )
    }
}
