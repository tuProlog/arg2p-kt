package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class Arg2pLibraryTest {
    @Test
    fun theVersionOfTheBuild() {
        // a release (0.17.0) or a development version (0.17.1-dev01-abcdef0)
        assertTrue(Regex("""\d+\.\d+\.\d+.*""").matches(Arg2pLibrary.version()), Arg2pLibrary.version())
    }

    @Test
    fun theCausalityModuleIsThere() {
        val arg2p = Arg2pLibrary.get()
        val solver =
            Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
                otherLibraries = arg2p.to2pLibraries().plus(FlagsBuilder().create().content()),
                staticKb = Theory.parse("r1 :=> a.\nr2 : a => b.", arg2p.operators()),
            )
        val solution = solver.solve(Struct.parse("causality::ness(X, a, b)", solver.operators)).first()
        assertTrue(solution.isYes)
        // the negation of a is a NESS cause of b
        assertEquals(Struct.parse("['-'(a)]"), solution.substitution.getByName("X"))
    }
}
