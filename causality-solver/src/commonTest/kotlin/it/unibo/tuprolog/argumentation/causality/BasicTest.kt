package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

class BasicTest {
    @Test
    fun causalityTest() {
        arg2pScope {
            ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
                otherLibraries = Arg2pSolver.causality().to2pLibraries(),
                flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
            ).also {
                it.loadStaticKb(Theory.parse("ciao.", it.operators))
                val a = it.solve("solve"("pippo")).first()
                assertEquals(a.isYes, true)
            }
        }
    }
}
