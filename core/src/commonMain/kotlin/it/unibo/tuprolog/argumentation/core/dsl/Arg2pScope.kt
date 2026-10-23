package it.unibo.tuprolog.argumentation.core.dsl

import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.dsl.solve.LogicProgrammingScope
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.SolverFactory
import it.unibo.tuprolog.solve.channel.InputChannel
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.exception.Warning
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

interface PrologWithArgumentation : LogicProgrammingScope {
    infix fun Any.call(other: Any) = structOf("::", this.toTerm(), other.toTerm())
}

/**
 * The solvers of the scopes of [arg2pScope]: each scope comes with a solver of its own, but arg2p only uses the
 * scopes to build terms (solving through the solvers and requests it is given), and their primitives open a scope at
 * each call: the solver of the scopes is created once, rather than with each scope (creating a solver cost as much as
 * solving the goals of most of the primitives).
 */
private object ScopeSolverFactory : SolverFactory by Arg2pPrologSolverFactory {
    private val solver: MutableSolver by lazy { Arg2pPrologSolverFactory.mutableSolverOf() }

    override fun mutableSolverOf(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        stdIn: InputChannel<String>,
        stdOut: OutputChannel<String>,
        stdErr: OutputChannel<String>,
        warnings: OutputChannel<Warning>,
    ): MutableSolver = solver
}

internal class PrologWithArgumentationImpl :
    PrologWithArgumentation,
    LogicProgrammingScope by LogicProgrammingScope.of(ScopeSolverFactory)

fun <R> arg2pScope(function: PrologWithArgumentation.() -> R): R = PrologWithArgumentationImpl().function()
