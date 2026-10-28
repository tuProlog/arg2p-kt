package it.unibo.tuprolog.argumentation.behaviour

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.behaviour.Behaviour.answerQuery
import it.unibo.tuprolog.argumentation.behaviour.Behaviour.buildLabelSets
import it.unibo.tuprolog.argumentation.behaviour.Behaviour.solver
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class PreferencesTest {
    private val baseTheory: String =
        """
        r0 : [] => c.
        r1 : c => a.
        r2 : [] => b.
        r3 : b => -a.
        r4 : a => d.
        r5 : -a => e.
        
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        graphExtension(rebutRestriction).
        graphExtension(standardPref).
        
        """.trimIndent()

    private val undResults =
        mapOf(
            "in" to "[[c],[b]]",
            "out" to "[]",
            "und" to "[[-a],[e],[d],[a]]",
        )

    private val definedResults =
        mapOf(
            "in" to "[[-a],[e],[c],[b]]",
            "out" to "[[d],[a]]",
            "und" to "[]",
        )

    private fun attacksSize(solver: Arg2p): Int =
        solver
            .solve("graphStore::attack(X, Y, Z, U)")
            .toList()
            .count { it.isYes }

    private fun check(
        theory: String,
        results: Map<String, String>,
        attacksNumber: Int,
    ) = buildLabelSets(theory, results["in"]!!, results["out"]!!, results["und"]!!).let {
        assertEquals(attacksNumber, attacksSize(it))
    }

    @Test
    fun noPreferences() =
        check(
            baseTheory +
                """
                orderingPrinciple(last).
                orderingComparator(elitist).
                """.trimIndent(),
            undResults,
            4,
        )

    @Test
    fun singlePreferenceLastElitist() =
        check(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(last).
                orderingComparator(elitist).
                """.trimIndent(),
            definedResults,
            2,
        )

    @Test
    fun singlePreferenceLastDemocrat() =
        check(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(last).
                orderingComparator(democrat).
                """.trimIndent(),
            definedResults,
            2,
        )

    @Test
    fun singlePreferenceLastNormal() =
        check(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(last).
                orderingComparator(normal).
                """.trimIndent(),
            definedResults,
            2,
        )

    @Test
    fun singlePreferenceWeakestElitist() =
        check(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(weakest).
                orderingComparator(elitist).
                """.trimIndent(),
            undResults,
            4,
        )

    @Test
    fun singlePreferenceWeakestDemocrat() =
        check(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(weakest).
                orderingComparator(democrat).
                """.trimIndent(),
            undResults,
            4,
        )

    // sup(r3, r2) and sup(r2, r1), closed transitively, prefer r3 (for -a) to r1 (for a), as sup(r3, r1) does
    @Test
    fun chainedPreferences() =
        check(
            baseTheory + "\nsup(r3, r2).\nsup(r2, r1).\norderingPrinciple(last).\norderingComparator(elitist).",
            definedResults,
            2,
        )

    // normal is defined on the last rules (Dung and Thang): with the weakest link it raises a domain error
    private fun weakestNormal(
        goal: String,
        flags: String = "",
    ) {
        val arg2p = solver(baseTheory + "sup(r3, r1).\norderingPrinciple(weakest).\norderingComparator(normal).\n$flags")
        val solution = arg2p.solve(goal).next()
        assertTrue(solution.isHalt && "domain_error(orderingPrinciple(last), weakest)" in solution.error.orEmpty(), solution.toString())
    }

    @Test
    fun singlePreferenceWeakestNormal() = weakestNormal("buildLabelSets(I, O, U)")

    @Test
    fun multiplePreferencesAWeakestElitist() =
        check(
            baseTheory +
                """
                sup(r3, r1).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(elitist).
                """.trimIndent(),
            definedResults,
            2,
        )

    @Test
    fun multiplePreferencesAWeakestDemocrat() =
        check(
            baseTheory +
                """
                sup(r3, r1).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(democrat).
                """.trimIndent(),
            undResults,
            4,
        )

    @Test
    fun multiplePreferencesBWeakestElitist() =
        check(
            baseTheory +
                """
                sup(r3, r0).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(elitist).
                """.trimIndent(),
            undResults,
            4,
        )

    @Test
    fun multiplePreferencesBWeakestDemocrat() =
        check(
            baseTheory +
                """
                sup(r3, r0).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(democrat).
                """.trimIndent(),
            definedResults,
            2,
        )

    @Test
    fun noPreferencesStructured() =
        answerQuery(
            baseTheory +
                """
                orderingPrinciple(last).
                orderingComparator(elitist).
                queryMode.
                """.trimIndent(),
            "e",
            "[]",
            "[]",
            "[e]",
        )

    @Test
    fun singlePreferenceLastElitistStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(last).
                orderingComparator(elitist).
                queryMode.
                """.trimIndent(),
            "e",
            "[e]",
            "[]",
            "[]",
        )

    @Test
    fun singlePreferenceLastDemocratStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(last).
                orderingComparator(democrat).
                queryMode.
                """.trimIndent(),
            "e",
            "[e]",
            "[]",
            "[]",
        )

    @Test
    fun singlePreferenceLastNormalStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(last).
                orderingComparator(normal).
                queryMode.
                """.trimIndent(),
            "e",
            "[e]",
            "[]",
            "[]",
        )

    @Test
    fun singlePreferenceWeakestElitistStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(weakest).
                orderingComparator(elitist).
                queryMode.
                """.trimIndent(),
            "e",
            "[]",
            "[]",
            "[e]",
        )

    @Test
    fun singlePreferenceWeakestDemocratStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r1).
                orderingPrinciple(weakest).
                orderingComparator(democrat).
                queryMode.
                """.trimIndent(),
            "e",
            "[]",
            "[]",
            "[e]",
        )

    @Test
    fun singlePreferenceWeakestNormalStructured() = weakestNormal("answerQuery(e, I, O, U)", "queryMode.")

    @Test
    fun multiplePreferencesAWeakestElitistStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r1).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(elitist).
                queryMode.
                """.trimIndent(),
            "e",
            "[e]",
            "[]",
            "[]",
        )

    @Test
    fun multiplePreferencesAWeakestDemocratStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r1).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(democrat).
                queryMode.
                """.trimIndent(),
            "e",
            "[]",
            "[]",
            "[e]",
        )

    @Test
    fun multiplePreferencesBWeakestElitistStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r0).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(elitist).
                queryMode.
                """.trimIndent(),
            "e",
            "[]",
            "[]",
            "[e]",
        )

    @Test
    fun multiplePreferencesBWeakestDemocratStructured() =
        answerQuery(
            baseTheory +
                """
                sup(r3, r0).
                sup(r2, r1).
                orderingPrinciple(weakest).
                orderingComparator(democrat).
                queryMode.
                """.trimIndent(),
            "e",
            "[e]",
            "[]",
            "[]",
        )
}
