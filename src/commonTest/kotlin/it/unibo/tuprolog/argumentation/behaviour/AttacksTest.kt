package it.unibo.tuprolog.argumentation.behaviour

import it.unibo.tuprolog.argumentation.behaviour.Behaviour.solver
import kotlin.test.Test
import kotlin.test.assertEquals

class AttacksTest {
    private val baseConfig =
        """
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        graphExtension(rebutRestriction).
        graphExtension(standardPref).
        orderingPrinciple(last).
        orderingComparator(elitist).
        
        """.trimIndent()

    private fun checkAttacks(
        theory: String,
        attacks: Map<String, Int>,
    ) {
        solver(theory).also { solver ->
            solver.solve("buildLabelSets").toList()
            attacks.forEach { (type, count) ->
                assertEquals(count, solver.solve("graphStore::attack($type, Y, Z, U)").toList().count { it.isYes }, type)
            }
        }
    }

    @Test
    fun rebut() {
        checkAttacks(
            baseConfig +
                """
                r1 : [] => a.
                r2 : a => b.
                r3 : [] => -a.
                """.trimIndent(),
            mapOf("rebut" to 3),
        )

        checkAttacks(
            baseConfig +
                """
                r1 : [] -> a.
                r2 : a -> b.
                r3 : [] -> -a.
                """.trimIndent(),
            mapOf("rebut" to 0),
        )
    }

    @Test
    fun undermine() {
        checkAttacks(
            baseConfig +
                """
                r1 :=> a.
                r2 : a => b.
                r3 : [] => -a.
                """.trimIndent(),
            mapOf("undermine" to 0, "rebut" to 1),
        )

        checkAttacks(
            baseConfig +
                """
                r1 :=> a.
                r2 : a => b.
                r3 :=> -a.
                """.trimIndent(),
            mapOf("undermine" to 3),
        )

        checkAttacks(
            baseConfig +
                """
                r1 :-> a.
                r2 : a -> b.
                r3 : [] -> -a.
                """.trimIndent(),
            mapOf("rebut" to 0, "undermine" to 0),
        )
    }

    @Test
    fun undercut() {
        checkAttacks(
            baseConfig +
                """
                r1 : [] => a.
                r2 : a => b.
                r3 : [] => undercut(r1).
                r4 :=> undercut(r2).
                """.trimIndent(),
            mapOf("undercut" to 3),
        )

        checkAttacks(
            baseConfig +
                """
                r1 :=> a.
                r2 : a => b.
                r3 : [] => undercut(r1).
                """.trimIndent(),
            mapOf("undercut" to 0),
        )

        checkAttacks(
            baseConfig +
                """
                r1 :-> a.
                r2 : a -> b.
                r3 : [] => undercut(r1).
                r4 : [] => undercut(r2).
                """.trimIndent(),
            mapOf("undercut" to 0),
        )
    }

    @Test
    fun contraryRebut() {
        checkAttacks(
            baseConfig +
                """
                r1 : ~(c) => a.
                r2 : a => b.
                r3 : [] => c.
                """.trimIndent(),
            mapOf("contrary_rebut" to 2),
        )

        checkAttacks(
            baseConfig +
                """
                r1 : ~(c) -> a.
                r2 : a -> b.
                r3 : [] -> c.
                """.trimIndent(),
            mapOf("contrary_rebut" to 2),
        )
    }

    @Test
    fun contraryUndermine() {
        checkAttacks(
            baseConfig +
                """
                r1 : ~(c) => a.
                r2 : a => b.
                r3 :=> c.
                """.trimIndent(),
            mapOf("contrary_undermine" to 2),
        )

        checkAttacks(
            baseConfig +
                """
                r1 : ~(c) -> a.
                r2 : a -> b.
                r3 :=> c.
                """.trimIndent(),
            mapOf("contrary_undermine" to 2),
        )
    }
}
