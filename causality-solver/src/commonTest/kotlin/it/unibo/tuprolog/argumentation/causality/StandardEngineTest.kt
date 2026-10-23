package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.causality.modules.CausalityModule
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.prolog.PrologSolverFactory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test

// The causality module on the solver of tuProlog (solve-prolog), and with no context_reset before its checks
class StandardEngineTest {
    private val theory = "tx2 : d => -r2.\nr1 :=> a.\nr2 : a => -b."

    private fun tuProlog(theory: String): MutableSolver {
        val arg2p = Arg2pSolver.default(listOf(FlagsBuilder().create(), CausalityModule()))
        return PrologSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries = arg2p.to2pLibraries(),
            // tuProlog's solver loses bindings a goal left to solve needs, unless every goal selected is tracked
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
            staticKb = Theory.parse(theory, arg2p.operators()),
        )
    }

    private fun checks(solver: MutableSolver) {
        isNessIntervention(solver, "[-a]", "-b")
        isNessIntervention(solver, "[-b]", "a", false)
        isNessOriginalIntervention(solver, "[d]", "-b")
        isNessCause(solver, "a", "-b")
        isNessCause(solver, "-d", "-b", false)
        isNessOriginalCause(solver, "-d", "-b")
        isButForCause(solver, "a", "-b")
    }

    @Test
    fun onTheSolverOfTuProlog() = checks(tuProlog(theory))

    @Test
    fun withNoContextReset() =
        Arg2pSolverFactory.causality(theory).let { solver ->
            isNessCause(solver, "a", "-b", reset = false)
            isNessCause(solver, "-d", "-b", false, reset = false)
            isButForCause(solver, "a", "-b", reset = false)
        }
}
