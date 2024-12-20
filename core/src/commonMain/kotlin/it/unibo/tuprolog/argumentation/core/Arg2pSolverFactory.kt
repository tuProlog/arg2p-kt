package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
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
    ) = Arg2pSolver.of(staticLibs + listOf(settings), dynamicLibs).let {
        ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries = it.to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
            staticKb = Theory.parse(theory, it.operators()),
        )
    }

    fun default(
        theory: String = "",
        settings: ArgLibrary = FlagsBuilder().create(),
        staticLibs: Iterable<ArgLibrary> = emptyList(),
        dynamicLibs: Iterable<ArgLibrary> = emptyList(),
    ) = Arg2pSolver.default(staticLibs + listOf(settings), dynamicLibs).let {
        ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries = it.to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
            staticKb = Theory.parse(theory, it.operators()),
        )
    }

    fun evaluate(kb: String, flags: FlagsBuilder): Sequence<Graph> =
        arg2pScope {
            default(
                theory = kb,
                settings = flags.create(),
            ).let { solver ->
                solver.solve(Struct.parse("buildLabelSetsSilent"))
                    .filter { it.isYes }
                    .map { solver.graph() }
            }
        }
}
