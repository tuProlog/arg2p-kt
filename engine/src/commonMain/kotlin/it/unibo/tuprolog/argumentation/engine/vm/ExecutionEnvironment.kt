package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.TimeInstant
import it.unibo.tuprolog.solve.channel.InputStore
import it.unibo.tuprolog.solve.channel.OutputStore
import it.unibo.tuprolog.solve.data.CustomDataStore
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.getAllOperators
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.toOperatorSet
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * What the primitives of a resolution see of it, shared by the [PrologExecutionContext] of each call: the solver state
 * they read and change through their side effects (the libraries, the flags, the knowledge bases, the operators, the
 * channels, the custom data), and what is fixed for the whole resolution (the unificator, the query, its start and its
 * time limit). A context refers to it rather than holding each value, as one is made for each call of a primitive. A
 * side effect gives the machine a new environment, which the calls after it see.
 *
 * @throws IllegalArgumentException if [startTime] or [maxDuration] are negative.
 */
data class ExecutionEnvironment(
    val unificator: Unificator = Unificator.default,
    val libraries: Runtime = Runtime.empty(),
    val flags: FlagStore = FlagStore.empty(),
    val staticKb: Theory = Theory.empty(unificator),
    val dynamicKb: MutableTheory = MutableTheory.empty(unificator),
    val operators: OperatorSet = getAllOperators(libraries, staticKb, dynamicKb).toOperatorSet(),
    val inputChannels: InputStore = InputStore.fromStandard(),
    val outputChannels: OutputStore = OutputStore.fromStandard(),
    val customData: CustomDataStore = CustomDataStore.empty(),
    val query: Struct = Truth.TRUE,
    val startTime: TimeInstant,
    val maxDuration: TimeDuration = TimeDuration.MAX_VALUE,
    /** The program of the machine the resolution runs on (see `engine.vm.VmSolver`): the solvers created for its sub-goals share it */
    val vm: Any? = null,
    /** Whether the theory of the user is visible to the resolution (not within a module call) */
    val theoryVisible: Boolean = true,
    /** The environment of the root of the resolution, where this one is that of a module call (null: this is the root) */
    val root: ExecutionEnvironment? = null,
) {
    init {
        require(startTime >= 0)
        require(maxDuration >= 0)
    }
}
