package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.SolverBuilder
import it.unibo.tuprolog.solve.SolverFactory
import it.unibo.tuprolog.solve.channel.InputChannel
import it.unibo.tuprolog.solve.channel.InputStore
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.channel.OutputStore
import it.unibo.tuprolog.solve.exception.Warning
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * The solvers of arg2p for the theory written in [text] in the editor: the IDE parses it, dropping its comments, so
 * they tag its clauses with the identifiers of their comments (see [AnnotatedSolver])
 */
internal class AnnotatedSolverFactory(
    private val text: String,
) : SolverFactory by Arg2pPrologSolverFactory {
    // A builder of this factory, not of the one of arg2p it delegates to
    override fun newBuilder(): SolverBuilder = super.newBuilder()

    override fun solverOf(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputs: InputStore,
        outputs: OutputStore,
    ): Solver = Arg2pPrologSolverFactory.solverOf(unificator, libraries, flags, annotated(staticKb), dynamicKb, inputs, outputs)

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
    ): Solver =
        Arg2pPrologSolverFactory.solverOf(
            unificator,
            libraries,
            flags,
            annotated(staticKb),
            dynamicKb,
            stdIn,
            stdOut,
            stdErr,
            warnings,
        )

    override fun mutableSolverOf(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputs: InputStore,
        outputs: OutputStore,
    ): MutableSolver =
        AnnotatedSolver(
            Arg2pPrologSolverFactory.mutableSolverOf(unificator, libraries, flags, annotated(staticKb), dynamicKb, inputs, outputs),
            text,
        )

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
    ): MutableSolver =
        AnnotatedSolver(
            Arg2pPrologSolverFactory.mutableSolverOf(
                unificator,
                libraries,
                flags,
                annotated(staticKb),
                dynamicKb,
                stdIn,
                stdOut,
                stdErr,
                warnings,
            ),
            text,
        )

    // A theory given on creation is that of the editor as well (if it is not, nothing is tagged)
    private fun annotated(theory: Theory) = RuleAnnotations.annotate(theory, text)
}
