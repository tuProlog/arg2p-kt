package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.graph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.prolog.PrologSolverFactory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The libraries of arg2p on the solver of tuProlog (solve-prolog) give the labels and the graphs of its own engine
class StandardEngineTest {
    private fun tuProlog(
        theory: String,
        flags: Flags,
    ): Solver {
        val arg2p = Arg2pSolver.default(listOf(flags.library()))
        return PrologSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries = arg2p.to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
            staticKb = Theory.parse(theory, arg2p.operators()),
        )
    }

    // Every solution of goal, as the bindings of its variables, with the graph mined after each one
    private fun results(
        solver: Solver,
        goal: String,
    ): List<String> =
        solver
            .solve(Struct.parse(goal, solver.operators))
            .filter { it.isYes }
            .map { solution ->
                val bindings = solution.substitution.entries.joinToString { "${it.key.name}=${it.value}" }
                val graph = solver.graph()
                "$bindings | ${graph.labellings.map { it.toString() }.sorted()} | ${graph.attacks.map { it.toString() }.sorted()}"
            }.toList()
            .map { it.replace(Regex("""_\d+"""), "_") }

    private fun assertSame(
        theory: String,
        goal: String,
        flags: Flags = Flags.defaults(),
    ) = assertEquals(
        results(Arg2pSolverFactory.default(theory = theory, settings = flags.library()), goal),
        results(tuProlog(theory, flags), goal),
        goal,
    )

    private val tweety = "d1 : bird(X) => flies(X).\nd2 : penguin(X) => bird(X).\ns1 : penguin(X) -> -flies(X).\na1 :-> penguin(tweety)."

    @Test
    fun theWholeGraph() = assertSame(tweety, "solve(In, Out, Und)", Flags.defaults().queryMode(false))

    @Test
    fun queryMode() = assertSame(tweety, "solve(flies(X), In, Out, Und)")

    @Test
    fun preferences() =
        assertSame(
            "r1 : a => b.\nr2 :=> -b.\nf1 :=> a.\nsup(r1, r2).",
            "solve(In, Out, Und)",
            Flags.defaults().queryMode(false),
        )

    @Test
    fun manyLabellings() =
        assertSame(
            "r1 :=> a.\nr2 :=> -a.\nr3 : a => b.",
            "solve(In, Out, Und)",
            Flags.parse("queryMode(off).\nargumentLabellingMode(preferred)."),
        )

    @Test
    fun burdenOfPersuasion() =
        assertSame(
            "bp(guilty).\nr1 : evidence => guilty.\nr2 : alibi => -guilty.\nf1 :=> evidence.\nf2 :=> alibi.",
            "solve(In, Out, Und)",
            Flags.parse("queryMode(off).\nargumentLabellingMode(bpGrounded).\ngraphExtension(standardPref).\ngraphExtension(bp)."),
        )
}
