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
        val theory =
            """
            tx2 : d => undercut(r2).
            
            r1 :-> a.
            r2 : a => b.
            
            """.trimIndent()

        arg2pScope {
            ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
                otherLibraries = Arg2pSolver.causality().to2pLibraries(),
                flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
                staticKb = Theory.parse(theory, Arg2pSolver.default().operators()),
            ).also {
                val a = it.solve("solve"("a", "b")).first()
                assertEquals(a.isYes, true)
                val b = it.solve("solve"("b", "a")).first()
                assertEquals(b.isYes, false)
                val c = it.solve("solve"("d", "b")).first()
                assertEquals(c.isYes, false)
                val d = it.solve("solve"("-d", "b")).first()
                assertEquals(d.isYes, true)
            }
        }
    }
}
