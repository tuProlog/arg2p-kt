package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.bp

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertTrue

/** The burdens of proof argued (the extension bp): a burden stands on each of its arguments */
class BpLibraryTest {
    private val config =
        "statementLabellingMode(statement).\nargumentLabellingMode(bpGrounded).\n" +
            "graphExtension(standardPref).\ngraphExtension(bp).\n"

    // Two arguments for the burden on guilty, one undercut: the burden stands on the other, and guilty, not met, is OUT
    @Test
    fun eachArgumentForABurdenGivesAnArgumentForItsBeingMetOfItsOwn() {
        val theory =
            "r1 : evidence => guilty.\nr2 : alibi => -guilty.\nf1 :=> evidence.\nf2 :=> alibi.\n" +
                "b1 : [] => bp(guilty).\nb2 : [] => bp(guilty).\nu : [] => undercut(b1).\n$config"
        val statOut = LibraryTesting(theory).value("solve(_, O, _)", "O")!!
        assertTrue("[guilty]" in statOut, statOut)
    }
}
