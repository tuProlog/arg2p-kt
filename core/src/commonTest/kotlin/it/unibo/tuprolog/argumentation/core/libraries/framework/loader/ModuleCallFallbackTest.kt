package it.unibo.tuprolog.argumentation.core.libraries.framework.loader

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.prolog.PrologSolverFactory
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Module calls solved by an engine other than the arg2p one (tuProlog's classic solver), which does not inline them:
 * the goal is solved in a solver of the module, binding the variables of the caller as the inlined call does.
 */
class ModuleCallFallbackTest {
    private val solver =
        Arg2pSolver.default(listOf(Flags.none().library())).let {
            PrologSolverFactory.mutableSolverWithDefaultBuiltins(
                unificator = Arg2pUnificator,
                otherLibraries = it.to2pLibraries(),
                // (see SubstitutionTrimmingTest)
                flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
            )
        }

    private fun binding(
        goal: String,
        variable: String,
    ): String {
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        return solution.substitution.entries
            .first { it.key.name == variable }
            .value
            .toString()
    }

    @Test
    fun moduleCall() = assertEquals("[b]", binding("utils::subtract([a, b], [a], L)", "L"))

    @Test
    fun bindingsInsideATermOfTheCaller() = assertEquals("f(1, b)", binding("S = f(_, b), utils::subtract([f(1, b)], [], [S])", "S"))

    // The module binds an anonymous variable of the caller to another: they stay bound to each other
    @Test
    fun anonymousVariablesBoundToEachOther() =
        assertEquals("f(a, a)", binding("S = f(_X, _Y), utils::subtract([_X], [], [_Y]), _X = a", "S"))

    @Test
    fun moduleCallInANewBranch() {
        val goal = "contextReset, contextActive(B0), utils:::true, contextActive(B1), (B0 \\== B1 -> L = yes ; L = no)"
        assertEquals("yes", binding(goal, "L"))
    }
}
