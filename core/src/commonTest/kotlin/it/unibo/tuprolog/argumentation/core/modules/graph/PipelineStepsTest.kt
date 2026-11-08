package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.argumentation.core.modules.ModuleRegistry
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The steps changing the graph are listed once, in their order (pipeline::steps/1): the graph extensions the
 * flag declares are those steps (but the copies of metaConflicts, which no flag names), and the registry loads them all
 */
class PipelineStepsTest {
    private val steps = ModuleTesting("").values("pipeline::steps(Steps), member(S, Steps)", "S")

    @Test
    fun theGraphExtensionsAreTheStepsAFlagNames() {
        assertEquals(
            (steps - "conflictArguments" - "constraints").toSet(),
            (GraphExtension.values() as Iterable<*>).map { it.toString() }.toSet(),
        )
    }

    @Test
    fun thePreferenceModesAreSteps() {
        val modes = ModuleTesting("").values("pipeline::preferenceMode(M)", "M")
        assertTrue(steps.containsAll(modes), "$modes among $steps")
    }

    @Test
    fun theRegistryLoadsEveryStep() {
        val loaded = ModuleRegistry.extensions.map { (it as Loadable).identifier() }
        assertTrue(loaded.containsAll(steps), "$steps among $loaded")
    }
}
