package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

object Arg2pSolverFactory {
    fun of(
        theory: String,
        settings: ArgLibrary,
        staticLibs: Iterable<ArgLibrary>,
        dynamicLibs: Iterable<ArgLibrary>,
    ) = solverOf(Arg2pSolver.of(staticLibs + listOf(settings), dynamicLibs), theory)

    fun default(
        theory: String = "",
        settings: ArgLibrary = FlagsBuilder().create(),
        staticLibs: Iterable<ArgLibrary> = emptyList(),
        dynamicLibs: Iterable<ArgLibrary> = emptyList(),
    ) = solverOf(Arg2pSolver.default(staticLibs + listOf(settings), dynamicLibs), theory)

    // A solver of the libraries of [arg2p] for [theory]: unknown predicates fail, variables are tracked
    private fun solverOf(
        arg2p: Arg2pSolver,
        theory: String,
    ) = Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
        unificator = Arg2pUnificator,
        otherLibraries = arg2p.to2pLibraries(),
        flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
        staticKb = Theory.parse(theory, arg2p.operators()),
    )

    fun evaluate(
        kb: String,
        flags: FlagsBuilder,
    ): Sequence<Graph> =
        arg2pScope {
            default(
                theory = kb,
                settings = flags.create(),
            ).let { solver ->
                solver
                    .solve(Struct.parse("buildLabelSetsSilent"))
                    .map { solver.graph() }
            }
        }
}
