// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0)
package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.engine.solver.MutableVmSolver
import it.unibo.tuprolog.argumentation.engine.solver.VmSolver
import it.unibo.tuprolog.argumentation.engine.stdlib.DefaultBuiltins
import it.unibo.tuprolog.argumentation.engine.vm.program.SharedLibrary
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.SolverFactory
import it.unibo.tuprolog.solve.channel.InputChannel
import it.unibo.tuprolog.solve.channel.InputStore
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.channel.OutputStore
import it.unibo.tuprolog.solve.exception.Warning
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator
import kotlin.js.JsExport

/**
 * The factory of the solvers of arg2p (a tuProlog `SolverFactory`): the solvers run on the machine of `engine.vm`,
 * with the builtins of [DefaultBuiltins] by default.
 */
@Suppress("NON_EXPORTABLE_TYPE")
@JsExport
object Arg2pPrologSolverFactory : SolverFactory {
    // (shared: the code of their clauses compiled once per process, as a module's)
    override val defaultBuiltins: Library by lazy { SharedLibrary.of(DefaultBuiltins) }

    override fun solverOf(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputs: InputStore,
        outputs: OutputStore,
    ): Solver = VmSolver(unificator, libraries, flags, staticKb, dynamicKb, inputs, outputs)

    override fun solverOf(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        stdIn: InputChannel<String>,
        stdOut: OutputChannel<String>,
        stdErr: OutputChannel<String>,
        warnings: OutputChannel<Warning>,
    ): Solver = VmSolver(unificator, libraries, flags, staticKb, dynamicKb, stdIn, stdOut, stdErr, warnings)

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
    ): MutableSolver = MutableVmSolver(unificator, libraries, flags, staticKb, dynamicKb, stdIn, stdOut, stdErr, warnings)

    override fun mutableSolverOf(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputs: InputStore,
        outputs: OutputStore,
    ): MutableSolver = MutableVmSolver(unificator, libraries, flags, staticKb, dynamicKb, inputs, outputs)
}
