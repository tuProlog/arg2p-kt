package it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals

// The evaluation of the whole graph: built, changed by the steps the flags select, labelled, in the context
class PipelineWholeTest {
    private val theory = "f1 :=> a.\nr1 : a => b.\nf2 :=> -b.\nsup(f2, r1).\ngraphExtension(standardPref).\n"

    @Test
    fun theStatementsOfTheWholeGraphAreLabelled() {
        val module = LibraryTesting(theory)
        assertEquals("[[['-'(b)], [a]], [[b]], []]", module.value("pipeline::statementLabels(I, O, U), L = [I, O, U]", "L"))
    }

    // the graph and its labels stay in the context: the arguments are those of the theory, the attack of r1 on f2 removed
    // by the preference
    @Test
    fun theGraphStaysInTheContext() {
        val module = LibraryTesting(theory)
        module.run("contextReset", "pipeline::evaluate")
        assertEquals("3", module.value("store::graphSize(N)", "N"))
        assertEquals("1", module.value("findall(T, store::directAttack(T, _, _), Ts), length(Ts, N)", "N"))
    }
}
