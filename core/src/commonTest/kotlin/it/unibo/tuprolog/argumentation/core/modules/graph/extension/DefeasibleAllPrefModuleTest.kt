package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertTrue

class DefeasibleAllPrefModuleTest {
    // -c rebuts c twice (from b(1) and from b(2)), both rebuttals invalidated by the same preference
    private val theory =
        """
        f1 :=> a.
        f2 :=> b(1).
        f3 :=> b(2).
        r1 : a => c.
        r2 : b(X) => -c.
        f4 :=> sup(r1, r2).
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(defeasibleAllPref).
        """.trimIndent()

    @Test
    fun theArgumentForTheSamePreferencesAttacksEachAttackTheyInvalidate() {
        val statIn = ModuleTesting(theory).value("buildLabelSets(I, _, _)", "I")!!
        assertTrue("[c]" in statIn, statIn)
    }
}
