package it.unibo.tuprolog.argumentation.core.modules.entry

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// solve(In, Out, Und), qualified (arg2p::solve) or not, labels the statements of the whole theory as
// buildLabelSets(In, Out, Und) does, each labelling a solution
class Arg2pSolveTest {
    // The labels of each solution of [goal] on [theory], as text
    private fun labellings(
        theory: String,
        goal: String,
        flags: FlagsBuilder,
    ): List<String> {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.create())
        return solver
            .solve(Struct.parse(goal, solver.operators))
            .onEach { assertTrue(!it.isHalt, "$goal halted: ${it.exception}") }
            .filter { it.isYes }
            .map { solution -> listOf("In", "Out", "Und").joinToString(" ") { "$it=${solution.substitution.getByName(it)}" } }
            .toList()
    }

    private fun assertSameAsBuildLabelSets(
        theory: String,
        flags: FlagsBuilder = FlagsBuilder(),
    ): List<String> {
        val expected = labellings(theory, "buildLabelSets(In, Out, Und)", flags)
        assertTrue(expected.isNotEmpty(), "no labelling of $theory")
        listOf("arg2p::solve(In, Out, Und)", "solve(In, Out, Und)").forEach {
            assertEquals(expected, labellings(theory, it, flags), it)
        }
        return expected
    }

    @Test
    fun aConflictWithAPreference() {
        assertSameAsBuildLabelSets("r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).")
    }

    @Test
    fun tweety() {
        assertSameAsBuildLabelSets(
            "d1 : bird(X) => flies(X).\nd2 : penguin(X) => bird(X).\ns1 : penguin(X) -> -flies(X).\na1 :-> penguin(tweety).",
        )
    }

    @Test
    fun anAbstractFramework() {
        assertSameAsBuildLabelSets("graphBuildMode(abstract).\na.\na -> b.\nb -> c.\nc <-> d.")
    }

    @Test
    fun severalLabellings() {
        val labellings = assertSameAsBuildLabelSets("r1 : [] => a.\nr2 : [] => -a.", FlagsBuilder().argumentLabellingMode("complete"))
        assertTrue(labellings.size > 1, "expected several labellings under complete, got $labellings")
    }
}
