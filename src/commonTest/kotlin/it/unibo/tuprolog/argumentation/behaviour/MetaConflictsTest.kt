package it.unibo.tuprolog.argumentation.behaviour

import it.unibo.tuprolog.argumentation.Value
import it.unibo.tuprolog.argumentation.behaviour.Behaviour.solver
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * With metaConflicts, an argument attacks through each argument for a conflict of its conclusion: through a conflict
 * whose argument is defeated it attacks no more, through the others it still does, whatever their order.
 */
class MetaConflictsTest {
    private val config =
        """
        metaConflicts.
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        """.trimIndent()

    // a conflicts with b and with c, x defeats the argument for conflict(a, b)
    private val statements = listOf("r1 : [] => a.", "r2 : [] => b.", "r3 : [] => c.", "r6 : [] => x.")
    private val conflicts = listOf("r4 : [] => conflict(a, b).", "r5 : [] => conflict(a, c).", "r7 : [] => conflict(x, conflict(a, b)).")

    // The statements IN, OUT and UND, each sorted
    private fun labels(rules: List<String>): List<String> {
        val solution = solver((rules + config).joinToString("\n")).solve("buildLabelSets(In, Out, Und)").next()
        return listOf("In", "Out", "Und").map { label ->
            (solution.value(label) as Value.List)
                .items
                .map { it.toString() }
                .sorted()
                .joinToString(" ")
        }
    }

    @Test
    fun eachConflictArgumentGivesItsOwnAttacks() {
        assertEquals(
            listOf("[a] [b] [conflict(a, c)] [conflict(x, conflict(a, b))] [x]", "[c] [conflict(a, b)]", ""),
            labels(statements + conflicts),
        )
    }

    @Test
    fun theOrderOfTheConflictsDoesNotMatter() {
        assertEquals(labels(statements + conflicts), labels(statements + conflicts.reversed()))
    }

    // The copy of a premise is undermined as the premise is: b falls, and c stands
    @Test
    fun theCopiesOfPremisesAreUndermined() {
        assertEquals(
            listOf("[a] [c] [conflict(a, b)] [conflict(b, c)]", "[b]", ""),
            labels(listOf("f1 :=> a.", "f2 :=> b.", "f3 :=> c.", "r4 : [] => conflict(a, b).", "r5 : [] => conflict(b, c).")),
        )
    }
}
