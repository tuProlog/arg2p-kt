package it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.LibraryTesting
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.graph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The part of the graph a goal depends on (query mode), as the semantics allows: its arguments, and the labels the
// whole graph gives them
class PipelineQueryTest {
    // b rests on a, which -a rebuts; c and d have nothing to do with b
    private val theory = "f1 :=> a.\nr1 : a => b.\nf2 :=> -a.\nf3 :=> c.\nr2 : c => d.\n"

    private fun conclusions(
        semantics: String,
        queryMode: Boolean,
        goal: String,
    ): Pair<Set<String>, String> {
        val solver =
            Arg2pSolverFactory.default(
                theory = theory + "argumentLabellingMode($semantics).\n",
                settings = Flags.defaults().queryMode(queryMode).library(),
            )
        val solution = solver.solve(Struct.parse("solve($goal, I, O, U)", solver.operators)).first()
        val labels = listOf("I", "O", "U").joinToString(" ") { solution.substitution.getByName(it).toString() }
        return solver
            .graph()
            .arguments
            .map { it.conclusion }
            .toSet() to labels
    }

    @Test
    fun aDirectionalSemanticsBuildsWhatTheGoalDependsOn() {
        val (partial, labels) = conclusions("grounded", true, "b")
        assertEquals(setOf("a", "b", "- a"), partial)
        assertEquals(conclusions("grounded", false, "b").second, labels)
    }

    @Test
    fun theSemanticsStatesItsLocality() {
        val locality = { semantics: String ->
            LibraryTesting("argumentLabellingMode($semantics).\n").value("pipeline::locality(L)", "L")
        }
        assertEquals("directional", locality("grounded"))
        assertEquals("connected", locality("naive"))
        assertEquals("wholeGraph", locality("stable"))
    }
}
