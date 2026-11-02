package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertTrue

/** The burdens of proof argued (the extension bp): a burden stands on each of its arguments */
class BpModuleTest {
    private val config =
        "statementLabellingMode(statement).\nargumentLabellingMode(bp_grounded).\n" +
            "graphExtension(standardPref).\ngraphExtension(bp).\n"

    // Two arguments for the burden on guilty, one undercut: the burden stands on the other, and guilty, not met, is OUT
    @Test
    fun eachArgumentForABurdenGivesAnArgumentForItsBeingMetOfItsOwn() {
        val theory =
            "r1 : evidence => guilty.\nr2 : alibi => -guilty.\nf1 :=> evidence.\nf2 :=> alibi.\n" +
                "b1 : [] => bp(guilty).\nb2 : [] => bp(guilty).\nu : [] => undercut(b1).\n$config"
        val statOut = ModuleTesting(theory).value("buildLabelSets(_, O, _)", "O")!!
        assertTrue("[guilty]" in statOut, statOut)
    }
}
