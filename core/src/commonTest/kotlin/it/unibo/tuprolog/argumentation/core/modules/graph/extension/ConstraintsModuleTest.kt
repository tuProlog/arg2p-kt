package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// Statements over regions (graphExtension(constraints)), in the whole graph and in query mode
class ConstraintsModuleTest {
    private val flags = "graphExtension(standardPref).\ngraphExtension(constraints).\n"

    // f holds after 1; a point within the region is supported by its restriction, a point outside it by nothing
    private val region = "f1 :=> holdsAt(f, from(1)).\nr1 : holdsAt(f, 5) => ok.\nr2 : holdsAt(f, 0) => no.\n"

    // -f stated at 7: the region conflicts with it, the restrictions to 5 and 9 do not
    private val stated = "c1 :=> -holdsAt(f, 7).\n"

    // the region flows through r3 (T is bound to from(1)): its restriction to 5 rests on holdsAt(f, 5) alone
    private val derived = "r3 : holdsAt(f, T) => g(T).\nr4 : g(5) => ok5.\nr5 : g(7) => ok7.\n"

    // The label of each statement: in, out, und, or none when no argument concludes it
    private fun labels(
        theory: String,
        queryMode: Boolean,
        vararg statements: String,
    ): List<String> =
        statements.map { statement ->
            val solver = Arg2pSolverFactory.default(theory = theory + flags, settings = FlagsBuilder().queryMode(queryMode).create())
            val solution =
                if (queryMode) {
                    solver.solve(Struct.parse("answerQuery($statement, I, O, U)", solver.operators)).first()
                } else {
                    solver.solve(Struct.parse("buildLabelSets(I, O, U)", solver.operators)).first()
                }
            // buildLabelSets labels the statements, written [Claim]; answerQuery the claims
            val term = Struct.parse(if (queryMode) statement else "[$statement]", solver.operators)
            listOf("I" to "in", "O" to "out", "U" to "und")
                .firstOrNull { (name, _) ->
                    solution.substitution
                        .getByName(name)!!
                        .castToList()
                        .toList()
                        .any { it == term }
                }?.second ?: "none"
        }

    @Test
    fun aRegionSupportsTheNumbersWithinIt() {
        listOf(false, true).forEach { queryMode ->
            assertEquals(listOf("in", "none"), labels(region, queryMode, "ok", "no"), "queryMode $queryMode")
        }
    }

    @Test
    fun aConflictOnANumberIsAConflictOnThatNumberAlone() {
        listOf(false, true).forEach { queryMode ->
            assertEquals(
                listOf("in", "und", "und"),
                labels(region + stated, queryMode, "ok", "holdsAt(f, 7)", "holdsAt(f, from(1))"),
                "queryMode $queryMode",
            )
        }
    }

    // -f over (5, 8]: the regions overlap there, so 6 is contested and 9 is not
    @Test
    fun regionsConflictWhereTheyOverlap() {
        val overlapping = region + "c2 :=> -holdsAt(f, between(5, 8)).\nr6 : holdsAt(f, 6) => ok6.\nr7 : holdsAt(f, 9) => ok9.\n"
        listOf(false, true).forEach { queryMode ->
            assertEquals(listOf("und", "in"), labels(overlapping, queryMode, "ok6", "ok9"), "queryMode $queryMode")
        }
    }

    @Test
    fun aRestrictionRestsOnTheRestrictionsOfItsSubArguments() {
        listOf(false, true).forEach { queryMode ->
            assertEquals(listOf("in", "und"), labels(region + stated + derived, queryMode, "ok5", "ok7"), "queryMode $queryMode")
        }
    }
}
