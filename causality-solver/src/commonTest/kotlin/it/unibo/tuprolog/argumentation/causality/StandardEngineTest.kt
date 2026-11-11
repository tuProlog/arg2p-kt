package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.prolog.PrologSolverFactory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The causality module on the solver of tuProlog (solve-prolog), and with no contextReset before its checks
class StandardEngineTest {
    private val theory = "tx2 : d => -r2.\nr1 :=> a.\nr2 : a => -b."

    private fun tuProlog(theory: String): MutableSolver {
        val arg2p = Arg2pSolver.default(listOf(Flags.defaults().library(), CausalityLibrary()))
        return PrologSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries = arg2p.to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
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

    // A theory changed with no contextReset: the checks answer as on a fresh solver of the new theory, with no version of
    // the theory to tell the change (the solver of tuProlog has none)
    @Test
    fun aTheoryChangedWithNoContextReset() {
        val other = "r1 :=> a.\nr2 : a => b.\nr3 : c => -b.\nr4 :=> c."
        val goals =
            listOf(
                "ness(X, a, -b)",
                "ness(X, a, b)",
                "butFor(a, b)",
                "nessIntervention([-a], b)",
                "findall(Id-P, store::rule(Id, P, _), X)",
            )

        fun results(solver: MutableSolver) =
            goals.map { goal ->
                solver
                    .solve(Struct.parse(goal, solver.operators))
                    .map {
                        if (it.isYes) {
                            it.substitution.getByName("X")?.toString() ?: "yes"
                        } else if (it.isNo) {
                            "no"
                        } else {
                            "halt"
                        }
                    }.first()
            }
        val solver = tuProlog(theory)
        assertEquals(results(tuProlog(theory)), results(solver))
        solver.loadStaticKb(Theory.parse(other, solver.operators))
        assertEquals(results(tuProlog(other)), results(solver))
    }

    @Test
    fun withNoContextReset() =
        Arg2pSolverFactory.causality(theory).let { solver ->
            isNessCause(solver, "a", "-b", reset = false)
            isNessCause(solver, "-d", "-b", false, reset = false)
            isButForCause(solver, "a", "-b", reset = false)
        }
}
