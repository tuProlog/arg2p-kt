package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.TimeInstant
import it.unibo.tuprolog.solve.channel.InputStore
import it.unibo.tuprolog.solve.channel.OutputStore
import it.unibo.tuprolog.solve.data.CustomDataStore
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.sideffects.SideEffect
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * The execution context the primitives of the libraries are called in (tuProlog's `ExecutionContext`): the
 * [environment] of the resolution (its libraries, flags, knowledge bases, channels, custom data, query and time
 * limit) and the [procedure] being called. The machine (`engine.vm`) keeps the bindings of a resolution in its own
 * heap: the goal a primitive is given has them applied, so the substitution a primitive reads is empty.
 */
internal data class PrologExecutionContext(
    val environment: ExecutionEnvironment,
    override val procedure: Struct? = null,
) : ExecutionContext {
    override val unificator: Unificator
        get() = environment.unificator

    override val libraries: Runtime
        get() = environment.libraries

    override val flags: FlagStore
        get() = environment.flags

    override val staticKb: Theory
        get() = environment.staticKb

    override val dynamicKb: MutableTheory
        get() = environment.dynamicKb

    override val operators: OperatorSet
        get() = environment.operators

    override val inputChannels: InputStore
        get() = environment.inputChannels

    override val outputChannels: OutputStore
        get() = environment.outputChannels

    override val customData: CustomDataStore
        get() = environment.customData

    override val startTime: TimeInstant
        get() = environment.startTime

    override val maxDuration: TimeDuration
        get() = environment.maxDuration

    override val substitution: Substitution.Unifier
        get() = Substitution.empty()

    override val logicStackTrace: List<Struct>
        get() = listOfNotNull(procedure)

    /** The context of the root of the resolution: the libraries of the user are its (a module call sees its own) */
    val root: PrologExecutionContext
        get() = environment.root?.let { PrologExecutionContext(it) } ?: this

    override fun createSolver(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputChannels: InputStore,
        outputChannels: OutputStore,
    ): Solver =
        VmSolver(unificator, libraries, flags, staticKb, dynamicKb, inputChannels, outputChannels, trustKb = true).also {
            environment.program?.let { program -> it.share(program, environment.theoryVisible) }
        }

    override fun createMutableSolver(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputChannels: InputStore,
        outputChannels: OutputStore,
    ): MutableSolver =
        MutableVmSolver(unificator, libraries, flags, staticKb, dynamicKb, inputChannels, outputChannels, trustKb = true).also {
            environment.program?.let { program -> it.share(program, environment.theoryVisible) }
        }

    override fun update(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        operators: OperatorSet,
        inputChannels: InputStore,
        outputChannels: OutputStore,
        customData: CustomDataStore,
    ): PrologExecutionContext =
        copy(
            environment =
                environment.copy(
                    unificator = unificator,
                    libraries = libraries,
                    flags = flags,
                    staticKb = staticKb,
                    dynamicKb = dynamicKb.toMutableTheory(),
                    operators = operators,
                    inputChannels = inputChannels,
                    outputChannels = outputChannels,
                    customData = customData,
                ),
        )

    override fun apply(sideEffect: SideEffect): PrologExecutionContext = super.apply(sideEffect) as PrologExecutionContext

    override fun apply(sideEffects: Iterable<SideEffect>): PrologExecutionContext = super.apply(sideEffects) as PrologExecutionContext

    override fun apply(sideEffects: Sequence<SideEffect>): PrologExecutionContext = super.apply(sideEffects) as PrologExecutionContext

    override fun toString(): String = "PrologExecutionContext(procedure=$procedure, startTime=$startTime, maxDuration=$maxDuration)"
}

/** The libraries of the root of the resolution of this context (those of the user: a module call sees its own) */
@InternalArg2pApi
val ExecutionContext.rootLibraries: Runtime
    get() = (this as? PrologExecutionContext)?.root?.libraries ?: libraries
