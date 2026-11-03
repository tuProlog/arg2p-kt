package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.fsm.TheoryIndexes
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
 * The part of a [PrologExecutionContext] the frames of a resolution share (arg2p): the solver state the primitives
 * see and change through their side effects (the libraries, the flags, the knowledge bases, the operators, the
 * channels, the custom data), and what is fixed for the whole resolution (the unificator, the query, its start and
 * its time limit, the indexes of the theories). A frame refers to it, rather than carrying each value: the engine
 * copies a frame at almost every step (a new frame for a goal, the frame left at its end, a frame resumed
 * backtracking), and copied these values with it, two thirds of the frame. A side effect gives the frame a new
 * environment, which the frames after it go on with (see [carried]): a resolution without side effects shares one.
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
    val theoryIndexes: TheoryIndexes = TheoryIndexes(),
) {
    init {
        require(startTime >= 0)
        require(maxDuration >= 0)
    }

    /**
     * This environment as a frame goes on with it, with [libraries] and [customData] in the place of its own: the
     * environment itself where they are its own, [candidate] where it holds exactly the resulting values (the
     * environment of the frame left, or of the frame resumed), a copy otherwise. The frame left at the end of an
     * inlined call keeps the libraries of the frame it returns to, and a frame resumed backtracking drops the
     * ephemeral data: with no side effect between, the environment before the call, or the one of the choice point,
     * is the result, and is reused rather than built again.
     */
    fun carried(
        libraries: Runtime = this.libraries,
        customData: CustomDataStore = this.customData,
        candidate: ExecutionEnvironment? = null,
    ): ExecutionEnvironment =
        when {
            libraries === this.libraries && customData === this.customData -> this
            candidate != null &&
                candidate.libraries === libraries &&
                candidate.customData === customData &&
                candidate.sharesTheRestWith(this) -> candidate
            else -> copy(libraries = libraries, customData = customData)
        }

    // Whether every value but the libraries and the custom data is the same as in [other]
    private fun sharesTheRestWith(other: ExecutionEnvironment): Boolean =
        unificator === other.unificator &&
            flags === other.flags &&
            staticKb === other.staticKb &&
            dynamicKb === other.dynamicKb &&
            operators === other.operators &&
            inputChannels === other.inputChannels &&
            outputChannels === other.outputChannels &&
            query === other.query &&
            startTime == other.startTime &&
            maxDuration == other.maxDuration &&
            theoryIndexes === other.theoryIndexes
}

/** This store without its ephemeral data: itself when it has none (`discardEphemeral` copies it in any case) */
internal fun CustomDataStore.withoutEphemeral(): CustomDataStore = if (ephemeral.isEmpty()) this else discardEphemeral()

/** This store with its persistent data alone: itself when it has nothing else (`preservePersistent` copies it in any case) */
internal fun CustomDataStore.persistentOnly(): CustomDataStore =
    if (durable.isEmpty() && ephemeral.isEmpty()) this else preservePersistent()
