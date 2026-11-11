package it.unibo.tuprolog.argumentation.core.libraries.language.aspicLanguage

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertTrue

// The language of ASPIC+ (graphBuildMode(aspic)): a theory is well formed, no contrary of an axiom or of the conclusion
// of a strict rule
class AspicLanguageLibraryTest {
    private fun solved(theory: String) =
        Arg2pSolverFactory
            .default(theory = theory + "graphBuildMode(aspic).\n", settings = Flags.defaults().library())
            .let { it.solve(Struct.parse("solve(I, O, U)", it.operators)).first() }

    @Test
    fun aContraryOfAStrictConclusionIsRefused() {
        val refused = solved("f1 :-> a.\ns1 : a -> b.\nf2 :=> c.\nconflict([c], [b]).\n")
        assertTrue(refused.isHalt && "well_formed_theory" in refused.exception.toString(), refused.toString())
    }

    // a conflict declared both ways is a contradiction, not a contrary
    @Test
    fun aConflictBothWaysIsNoContrary() {
        assertTrue(solved("f1 :-> a.\ns1 : a -> b.\nf2 :=> c.\nconflict([c], [b]).\nconflict([b], [c]).\n").isYes)
    }

    @Test
    fun aContraryOfADefeasibleConclusionIsAccepted() {
        assertTrue(solved("f1 :=> a.\nr1 : a => b.\nf2 :=> c.\nconflict([c], [b]).\n").isYes)
    }
}
