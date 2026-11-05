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

    // Two arguments for the same preference, one undercut: the preference stands on the other, and so does c
    @Test
    fun eachArgumentForThePreferencesGivesAPreferenceArgumentOfItsOwn() {
        val two =
            "f1 :=> a.\nr1 : [] => c.\nr2 : [] => -c.\np1 : [] => sup(r1, r2).\np2 : [] => sup(r1, r2).\nu : [] => undercut(p1).\n" +
                "statementLabellingMode(statement).\nargumentLabellingMode(grounded).\ngraphExtension(defeasibleAllPref).\n"
        val statIn = ModuleTesting(two).value("buildLabelSets(I, _, _)", "I")!!
        assertTrue("[c]" in statIn, statIn)
        assertTrue("[preference([sup(r1, r2)])]" in statIn, statIn)
    }
}
