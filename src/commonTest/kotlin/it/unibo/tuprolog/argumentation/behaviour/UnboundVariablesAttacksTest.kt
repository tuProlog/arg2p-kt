package it.unibo.tuprolog.argumentation.behaviour

import it.unibo.tuprolog.argumentation.behaviour.Behaviour.solver
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// Arguments containing unbound variables must keep the same identifier across the attacks they are involved in,
// otherwise the labelling ignores those attacks. An open conclusion (undercut(r(x, _)), b(_)) is derived only with the
// graph extension constraints
class UnboundVariablesAttacksTest {
    private val groundedTheory: String =
        """
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).

        """.trimIndent()

    private fun labels(
        theory: String,
        query: String? = null,
    ): Map<String, Set<String>> =
        solver(groundedTheory + theory + (if (query != null) "\nqueryMode." else "")).let { solver ->
            solver.solve(if (query != null) "answerQuery($query)" else "buildLabelSets").next()
            solver
                .graph()
                .labellings
                .groupBy({ it.argument.topRule.substringBefore("(") }, { it.label })
                .mapValues { it.value.toSet() }
        }

    private fun assertLabel(
        labels: Map<String, Set<String>>,
        rule: String,
        label: String,
    ) {
        assertTrue(rule in labels, "No argument with top rule $rule in $labels")
        assertEquals(setOf(label), labels[rule], "Wrong label for arguments with top rule $rule")
    }

    private val unboundUndercutTheory: String =
        """
        graphExtension(standardPref).
        graphExtension(rebutRestriction).
        graphExtension(constraints).
        f1 :-> a.
        f2 :-> c(x, y).
        u : a => undercut(r(x, _)).
        r(X, Y) : a, c(X, Y) => b(X).
        """.trimIndent()

    private val unboundRebutTheory: String =
        """
        graphExtension(standardPref).
        graphExtension(constraints).
        orderingPrinciple(last).
        orderingComparator(elitist).
        f1 :-> a.
        r : a => b(_).
        u : a => -b(z).
        sup(u, r).
        """.trimIndent()

    @Test
    fun undercutWithUnboundAttacker() =
        labels(unboundUndercutTheory).let {
            assertLabel(it, "u", "in")
            assertLabel(it, "r", "out")
        }

    @Test
    fun undercutWithUnboundAttackerInQueryMode() =
        labels(unboundUndercutTheory, "b(x)").let {
            assertLabel(it, "u", "in")
            assertLabel(it, "r", "out")
        }

    @Test
    fun undercutWithGroundAttacker() =
        labels(
            """
            f1 :-> a.
            f2 :-> c(x, y).
            u : a => undercut(r(x, y)).
            r(X, Y) : a, c(X, Y) => b(X).
            """.trimIndent(),
        ).let {
            assertLabel(it, "u", "in")
            assertLabel(it, "r", "out")
        }

    // without the extension r concludes b(_) for no value in particular: no argument
    @Test
    fun anOpenConclusionNeedsTheExtension() =
        labels(unboundRebutTheory.replace("graphExtension(constraints).\n", "")).let {
            assertLabel(it, "u", "in")
            assertTrue("r" !in it, "An argument with top rule r in $it")
        }

    @Test
    fun rebutWithUnboundTarget() =
        labels(unboundRebutTheory).let {
            assertLabel(it, "u", "in")
            assertLabel(it, "r", "out")
        }

    @Test
    fun rebutWithUnboundTargetInQueryMode() =
        labels(unboundRebutTheory, "b(X)").let {
            assertLabel(it, "u", "in")
            assertLabel(it, "r", "out")
        }
}
