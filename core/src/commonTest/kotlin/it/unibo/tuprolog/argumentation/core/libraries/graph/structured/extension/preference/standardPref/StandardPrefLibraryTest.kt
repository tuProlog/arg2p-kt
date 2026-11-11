package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.standardPref

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals

// The preference mode of ASPIC+: an attack fails where the attacker is weaker than what it attacks (the ordering of the
// flags, last link by default, elitist)
class StandardPrefLibraryTest {
    private fun labels(theory: String) =
        LibraryTesting(theory + "argumentLabellingMode(grounded).\n")
            .value("solve(I, O, U), L = [I, O, U]", "L")

    @Test
    fun thePreferredRuleWins() {
        assertEquals("[[[a]], [['-'(a)]], []]", labels("r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\ngraphExtension(standardPref).\n"))
    }

    @Test
    fun withNoPreferenceTheRebuttalStands() {
        assertEquals("[[], [], [['-'(a)], [a]]]", labels("r1 : [] => a.\nr2 : [] => -a.\ngraphExtension(standardPref).\n"))
    }

    // last link: the rules last used decide (r3 and r2), whatever r1 under r3
    @Test
    fun theLastLinkDecides() {
        val theory = "r1 : [] => b.\nr3 : b => a.\nr2 : [] => -a.\nsup(r3, r2).\nsup(r2, r1).\ngraphExtension(standardPref).\n"
        assertEquals("[[[b], [a]], [['-'(a)]], []]", labels(theory))
    }
}
