package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.parsing.parse

object Arg2pSolverFactory {
    fun of(
        theory: String,
        settings: ArgLibrary,
        staticLibs: Iterable<ArgLibrary>,
        dynamicLibs: Iterable<ArgLibrary>,
    ) = solverOf(Arg2pSolver.of(staticLibs + listOf(settings), dynamicLibs), theory)

    /** A solver of the libraries of arg2p (see Arg2pSolver.default) for [theory], with [settings] */
    fun default(
        theory: String = "",
        settings: ArgLibrary = FlagsBuilder().create(),
        staticLibs: Iterable<ArgLibrary> = emptyList(),
        dynamicLibs: Iterable<ArgLibrary> = emptyList(),
        sandboxed: Boolean = false,
    ) = solverOf(Arg2pSolver.default(staticLibs + listOf(settings), dynamicLibs, sandboxed), theory)

    /**
     * A solver of the libraries of [arg2p], and the [extra] ones, for [theory] (its clauses tagged with the
     * identifiers of their comments, see RuleAnnotations): unknown predicates fail (the hooks a theory may define,
     * as conflict/2 and sup/2, are called whether it does or not), what it prints goes to [output] (the standard
     * output by default)
     */
    fun solverOf(
        arg2p: Arg2pSolver,
        theory: String,
        extra: Runtime = Runtime.empty(),
        output: OutputChannel<String> = OutputChannel.stdOut(),
    ) = Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
        unificator = Arg2pUnificator,
        otherLibraries = arg2p.to2pLibraries() + extra,
        flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
        staticKb = RuleAnnotations.parse(theory, arg2p.operators()),
        stdOut = output,
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
