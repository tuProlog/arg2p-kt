package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import kotlin.test.Test
import kotlin.test.assertEquals

class AttacksTest {

    private val baseConfig =
        """
        graphBuildMode(base).
        statementLabellingMode(base).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        
        """.trimIndent()

    private fun checkAttacks(theory: String, attacks: Map<String, Int>) {
        prolog {
            TestingUtils.solverWithTheory(theory).also { solver ->
                solver.solve(Struct.parse("buildLabelSets")).toList()
                attacks.forEach { attack ->
                    assertEquals(
                        attack.value,
                        solver.solve("attack"(attack.key, "Y", "Z")).filter { it.isYes }.count()
                    )
                }
            }
        }
    }

    @Test
    fun rebut() {
        checkAttacks(
            baseConfig + """
                r1 : [] => a.
                r2 : a => b.
                r3 : [] => -a.
            """.trimIndent(),
            mapOf("rebut" to 3)
        )

        checkAttacks(
            baseConfig + """
                r1 : [] -> a.
                r2 : a -> b.
                r3 : [] -> -a.
            """.trimIndent(),
            mapOf("rebut" to 0)
        )
    }

    @Test
    fun undermine() {
        checkAttacks(
            baseConfig + """
                r1 :=> a.
                r2 : a => b.
                r3 : [] => -a.
            """.trimIndent(),
            mapOf("undermine" to 1, "rebut" to 1)
        )

        checkAttacks(
            baseConfig + """
                r1 :=> a.
                r2 : a => b.
                r3 :=> -a.
            """.trimIndent(),
            mapOf("undermine" to 3)
        )

        checkAttacks(
            baseConfig + """
                r1 :-> a.
                r2 : a -> b.
                r3 : [] -> -a.
            """.trimIndent(),
            mapOf("rebut" to 0, "undermine" to 0)
        )
    }

    @Test
    fun undercut() {
        checkAttacks(
            baseConfig + """
                r1 : [] => a.
                r2 : a => b.
                r3 : [] => undercut(r1).
                r4 :=> undercut(r2).
            """.trimIndent(),
            mapOf("undercut" to 3)
        )

        checkAttacks(
            baseConfig + """
                r1 :=> a.
                r2 : a => b.
                r3 : [] => undercut(r1).
            """.trimIndent(),
            mapOf("undercut" to 0)
        )

        checkAttacks(
            baseConfig + """
                r1 :-> a.
                r2 : a -> b.
                r3 : [] => undercut(r1).
                r4 : [] => undercut(r2).
            """.trimIndent(),
            mapOf("undercut" to 0)
        )
    }

    @Test
    fun contraryRebut() {
        checkAttacks(
            baseConfig + """
                r1 : ~(c) => a.
                r2 : a => b.
                r3 : [] => c.
            """.trimIndent(),
            mapOf("contrary_rebut" to 2)
        )

        checkAttacks(
            baseConfig + """
                r1 : ~(c) -> a.
                r2 : a -> b.
                r3 : [] -> c.
            """.trimIndent(),
            mapOf("contrary_rebut" to 0)
        )
    }

    @Test
    fun contraryUndermine() {
        checkAttacks(
            baseConfig + """
                r1 : ~(c) => a.
                r2 : a => b.
                r3 :=> c.
            """.trimIndent(),
            mapOf("contrary_undermine" to 2)
        )

        checkAttacks(
            baseConfig + """
                r1 : ~(c) -> a.
                r2 : a -> b.
                r3 :-> c.
            """.trimIndent(),
            mapOf("contrary_undermine" to 0)
        )
    }
}
