package it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import it.unibo.tuprolog.argumentation.core.libraries.LibraryRegistry
import it.unibo.tuprolog.argumentation.core.libraries.Loadable
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.GraphExtension
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The steps changing the graph are listed once, in their order (phases::steps/1): the graph extensions the
 * flag declares are those steps (but the copies of metaConflicts, which no flag names), and the registry loads them all
 */
class PhasesStepsTest {
    private val steps = LibraryTesting("").values("phases::steps(Steps), member(S, Steps)", "S")

    @Test
    fun theGraphExtensionsAreTheStepsAFlagNames() {
        assertEquals(
            (steps - "conflictArguments" - "constraints").toSet(),
            GraphExtension.values().toSet(),
        )
    }

    @Test
    fun thePreferenceModesAreSteps() {
        val modes = LibraryTesting("").values("phases::preferenceMode(M)", "M")
        assertTrue(steps.containsAll(modes), "$modes among $steps")
    }

    @Test
    fun theRegistryLoadsEveryStep() {
        val loaded = LibraryRegistry.extensions.map { (it as Loadable).identifier() }
        assertTrue(loaded.containsAll(steps), "$steps among $loaded")
    }
}
