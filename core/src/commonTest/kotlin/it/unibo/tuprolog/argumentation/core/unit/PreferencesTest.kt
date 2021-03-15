package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.yes
import kotlin.test.Test
import kotlin.test.assertEquals

class PreferencesTest {

    private fun solverWithTheory(theory: String) = TestingUtils.solver(TestingUtils.withArgOperators(theory))

    private val baseTheory: String = """
            r0 : [] => c.
            r1 : c => a.
            r2 : [] => b.
            r3 : b => -a.
            r4 : a => d.
            r5 : -a => e.
            
            graphBuildMode(base).
            statementLabellingMode(base).
            argumentLabellingMode(grounded).
            
    """.trimIndent()

    private val undResults = mapOf(
        "in" to "[[c],[b]]",
        "out" to "[]",
        "und" to "[[neg,a],[e],[d],[a]]"
    )

    private val definedResults = mapOf(
        "in" to "[[neg,a],[e],[c],[b]]",
        "out" to "[[d],[a]]",
        "und" to "[]"
    )

    private fun buildLabelSets(theory: String, argsIn: String, argsOut: String, argsUnd: String) : MutableSolver {
        return prolog {
            solverWithTheory(theory).also { solver ->
                TestingUtils.testGoalNoBacktracking(
                    "buildLabelSets"("StatIn", "StatOut", "StatUnd"),
                    solver
                ) {
                    it.yes(
                        "StatIn" to Struct.parse(argsIn),
                        "StatOut" to Struct.parse(argsOut),
                        "StatUnd" to Struct.parse(argsUnd)
                    )
                }
            }
        }
    }

    private fun attacksSize(solver: MutableSolver) : Int = prolog {
            solver.solve("attack"("X", "Y", "Z"))
                .filter { it.isYes }.count() }

    private fun check(theory: String, results: Map<String, String>, attacksNumber: Int) =
        buildLabelSets(theory, results["in"]!!, results["out"]!!, results["und"]!!).let {
            assertEquals(attacksNumber, attacksSize(it)) }

    private fun answerQuery(theory: String, query: String, argsIn: String, argsOut: String, argsUnd: String) {
        prolog {
            solverWithTheory(theory).also { solver ->
                TestingUtils.testGoalNoBacktracking(
                    "answerQuery"(query,"StatIn", "StatOut", "StatUnd"),
                    solver
                ) {
                    it.yes(
                        "StatIn" to Struct.parse(argsIn),
                        "StatOut" to Struct.parse(argsOut),
                        "StatUnd" to Struct.parse(argsUnd)
                    )
                }
            }
        }
    }

    @Test
    fun noPreferences() = check(baseTheory + """
            orderingPrinciple(last).
            orderingComparator(elitist).
        """.trimIndent(), undResults, 4)

    @Test
    fun singlePreferenceLastElitist() = check(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(last).
            orderingComparator(elitist).
        """.trimIndent(), definedResults, 3)

    @Test
    fun singlePreferenceLastDemocrat() = check(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(last).
            orderingComparator(democrat).
        """.trimIndent(), definedResults, 3)

    @Test
    fun singlePreferenceLastNormal() = check(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(last).
            orderingComparator(normal).
        """.trimIndent(), definedResults, 2)

    @Test
    fun singlePreferenceWeakestElitist() = check(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(weakest).
            orderingComparator(elitist).
        """.trimIndent(), undResults, 4)

    @Test
    fun singlePreferenceWeakestDemocrat() = check(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(weakest).
            orderingComparator(democrat).
        """.trimIndent(), undResults, 4)

    @Test
    fun singlePreferenceWeakestNormal() = check(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(weakest).
            orderingComparator(normal).
        """.trimIndent(), definedResults, 2)

    @Test
    fun multiplePreferencesAWeakestElitist() = check(baseTheory + """
            sup(r3, r1).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(elitist).
        """.trimIndent(), definedResults, 3)

    @Test
    fun multiplePreferencesAWeakestDemocrat() = check(baseTheory + """
            sup(r3, r1).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(democrat).
        """.trimIndent(), undResults, 4)

    @Test
    fun multiplePreferencesBWeakestElitist() = check(baseTheory + """
            sup(r3, r0).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(elitist).
        """.trimIndent(), undResults, 4)

    @Test
    fun multiplePreferencesBWeakestDemocrat() = check(baseTheory + """
            sup(r3, r0).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(democrat).
        """.trimIndent(), definedResults, 2)

    @Test
    fun noPreferencesStructured() = answerQuery(baseTheory + """
            orderingPrinciple(last).
            orderingComparator(elitist).
            queryMode.
        """.trimIndent(), "e", "[]", "[]", "[[e]]")

    @Test
    fun singlePreferenceLastElitistStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(last).
            orderingComparator(elitist).
            queryMode.
        """.trimIndent(), "e", "[[e]]", "[]", "[]")

    @Test
    fun singlePreferenceLastDemocratStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(last).
            orderingComparator(democrat).
            queryMode.
        """.trimIndent(), "e", "[[e]]", "[]", "[]")

    @Test
    fun singlePreferenceLastNormalStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(last).
            orderingComparator(normal).
            queryMode.
        """.trimIndent(), "e", "[[e]]", "[]", "[]")

    @Test
    fun singlePreferenceWeakestElitistStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(weakest).
            orderingComparator(elitist).
            queryMode.
        """.trimIndent(), "e", "[]", "[]", "[[e]]")

    @Test
    fun singlePreferenceWeakestDemocratStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(weakest).
            orderingComparator(democrat).
            queryMode.
        """.trimIndent(), "e", "[]", "[]", "[[e]]")

    @Test
    fun singlePreferenceWeakestNormalStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            orderingPrinciple(weakest).
            orderingComparator(normal).
            queryMode.
        """.trimIndent(), "e", "[[e]]", "[]", "[]")

    @Test
    fun multiplePreferencesAWeakestElitistStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(elitist).
            queryMode.
        """.trimIndent(), "e", "[[e]]", "[]", "[]")

    @Test
    fun multiplePreferencesAWeakestDemocratStructured() = answerQuery(baseTheory + """
            sup(r3, r1).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(democrat).
            queryMode.
        """.trimIndent(), "e", "[]", "[]", "[[e]]")

    @Test
    fun multiplePreferencesBWeakestElitistStructured() = answerQuery(baseTheory + """
            sup(r3, r0).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(elitist).
            queryMode.
        """.trimIndent(), "e", "[]", "[]", "[[e]]")

    @Test
    fun multiplePreferencesBWeakestDemocratStructured() = answerQuery(baseTheory + """
            sup(r3, r0).
            sup(r2, r1).
            orderingPrinciple(weakest).
            orderingComparator(democrat).
            queryMode.
        """.trimIndent(), "e", "[[e]]", "[]", "[]")
}
