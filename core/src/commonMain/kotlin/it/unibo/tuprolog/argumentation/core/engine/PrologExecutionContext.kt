// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.fsm.collectVariables
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.Var
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
import it.unibo.tuprolog.solve.getAllOperators
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.sideffects.SideEffect
import it.unibo.tuprolog.solve.toOperatorSet
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator
import it.unibo.tuprolog.utils.Cursor
import kotlin.collections.List as KtList
import kotlin.collections.Set as KtSet

/**
 * One frame of the `:solve-prolog` execution-context stack: a substitution, the streams of remaining goals,
 * candidate [Rule]s and candidate primitive responses, plus a [parent] link. Frames chain into their [parent]
 * rather than living in a separate stack container, so the [parent] chain (see [pathToRoot]) *is* the call
 * stack -- ordinary heap-allocated data rather than native JVM/JS stack frames, which is what lets Prolog
 * resolution of arbitrarily deep recursive programs run as an explicit, non-recursive [it.unibo.tuprolog.argumentation.core.engine.fsm.State]
 * loop instead of unbounded host-language recursion.
 *
 * Resolving a sub-goal (a primitive call or a rule body) pushes a new context whose [parent] is the current one
 * (see `it.unibo.tuprolog.argumentation.core.engine.fsm.createChild`); finishing a context's [goals] pops back to its
 * [parent] in [it.unibo.tuprolog.argumentation.core.engine.fsm.StateGoalSelection]. Every instance also threads through
 * [choicePoints], the sibling data structure (see [ChoicePointContext]) recording still-open backtracking
 * alternatives.
 *
 * Most fields mirror [it.unibo.tuprolog.solve.ExecutionContext]; the ones specific to the prolog engine are:
 * - [query]/[goals]: the original query, and the cursor over the goals still to be proven in this frame.
 * - [rules]/[primitives]: the still-untried candidate clauses/primitive responses for the current goal, i.e. the
 *   frame's own view of the [choicePoints] entry recorded for it (if any).
 * - [choicePoints]: the head of the choice-point queue as seen from this frame -- see [ChoicePointContext].
 * - [parent]/[depth]: the enclosing frame and this frame's distance from the root (`depth == 0`, [isRoot]).
 * - [step]: a monotonically increasing counter of state-machine transitions, used to keep [PrologSolver]'s
 *   `currentContext` in sync with resolution progress and to detect [it.unibo.tuprolog.solve.exception.TimeOutException]s.
 * - [relevantGoals]: the goals selected so far in this frame, whose variables are still of interest, used to trim
 *   [substitution]s of variables local to a finished sub-goal when popping back to [parent] (see
 *   [isVariableInteresting]); [trimmedSize] is the size of the substitution when it was last trimmed.
 * - [inlined]: whether this frame solves the goal of an [InlinedCall], against libraries of its own; [scoped]: whether
 *   this frame or one above does (so that its libraries are not the ones of the whole resolution).
 *
 * @throws IllegalArgumentException if [depth] and [parent] are inconsistent (`depth == 0` iff `parent == null`),
 * or if [startTime] or [maxDuration] are negative.
 */
data class PrologExecutionContext(
    override val procedure: Struct? = null,
    override val unificator: Unificator = Unificator.default,
    override val libraries: Runtime = Runtime.empty(),
    override val flags: FlagStore = FlagStore.empty(),
    override val staticKb: Theory = Theory.empty(unificator),
    override val dynamicKb: MutableTheory = MutableTheory.empty(unificator),
    override val operators: OperatorSet = getAllOperators(libraries, staticKb, dynamicKb).toOperatorSet(),
    override val inputChannels: InputStore = InputStore.fromStandard(),
    override val outputChannels: OutputStore = OutputStore.fromStandard(),
    override val customData: CustomDataStore = CustomDataStore.empty(),
    override val substitution: Substitution.Unifier = Substitution.empty(),
    val query: Struct = Truth.TRUE,
    val goals: Cursor<out Term> = Cursor.empty(),
    val rules: Cursor<out Rule> = Cursor.empty(),
    val primitives: Cursor<out Solve.Response> = Cursor.empty(),
    override val startTime: TimeInstant,
    override val maxDuration: TimeDuration = TimeDuration.MAX_VALUE,
    val choicePoints: ChoicePointContext? = null,
    val parent: PrologExecutionContext? = null,
    val depth: Int = 0,
    val step: Long = 0,
    val relevantGoals: RelevantGoals? = null,
    val trimmedSize: Int = 0,
    val inlined: Boolean = false,
    val scoped: Boolean = false,
) : ExecutionContext {
    init {
        require((depth == 0 && parent == null) || (depth > 0 && parent != null))
        require(startTime >= 0)
        require(maxDuration >= 0)
    }

    /** Whether this is the outermost frame of a resolution (i.e. it has no [parent]). */
    val isRoot: Boolean
        get() = depth == 0

    /** Whether [choicePoints] (or any of its ancestors) still has an alternative to backtrack into. */
    val hasOpenAlternatives: Boolean
        get() = choicePoints?.hasOpenAlternatives ?: false

    /** Whether this frame corresponds to an actual procedure call, i.e. should appear in [logicStackTrace]. */
    @Suppress("MemberVisibilityCanBePrivate")
    val isActivationRecord: Boolean
        get() = parent == null || depth - parent.depth >= 1

    /**
     * This frame and all its ancestors, from here up to (and including) the root, in that order (arg2p: built when
     * needed, rather than for each of the contexts, one or more per resolution step).
     */
    val pathToRoot: Sequence<PrologExecutionContext>
        get() = generateSequence(this) { it.parent }

    /** The current [goals] entry with [substitution] already applied, or `null` if there is none left to prove. */
    val currentGoal: Term? by lazy(LazyThreadSafetyMode.NONE) {
        if (goals.isOver) null else goals.current?.apply(substitution)
    }

    // A set rather than a sequence: membership is checked for every binding of the substitution whenever a frame is
    // left, and a sequence is scanned at each check (arg2p: the scans were quadratic in deep resolutions)
    private val interestingVariables: KtSet<Var> by lazy(LazyThreadSafetyMode.NONE) {
        HashSet<Var>().also { variables ->
            query.collectVariables(variables)
            var context: PrologExecutionContext? = this
            while (context != null) {
                var relevant = context.relevantGoals
                while (relevant != null) {
                    relevant.goal.collectVariables(variables)
                    relevant = relevant.previous
                }
                context.goals.current?.collectVariables(variables)
                context = context.parent
            }
        }
    }

    /**
     * Whether [variable] is still relevant to this frame or any of its ancestors (i.e. occurs in [relevantGoals],
     * or occurs in this frame's [currentGoal] or [query], or in an ancestor's). Used when popping a finished
     * frame back to its [parent] (in [it.unibo.tuprolog.argumentation.core.engine.fsm.StateGoalSelection]) to filter the
     * [substitution] handed up, discarding bindings for variables local to the finished sub-goal.
     */
    fun isVariableInteresting(variable: Var) = variable in interestingVariables

    override val logicStackTrace: KtList<Struct> by lazy(LazyThreadSafetyMode.NONE) {
        pathToRoot
            .filter { it.isActivationRecord }
            .map { it.procedure ?: Struct.of("?-", query) }
            .toList()
    }

    override fun createSolver(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputChannels: InputStore,
        outputChannels: OutputStore,
    ): Solver =
        PrologSolver(
            unificator,
            libraries,
            flags,
            staticKb,
            dynamicKb,
            inputChannels,
            outputChannels,
            trustKb = true,
        )

    override fun createMutableSolver(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        inputChannels: InputStore,
        outputChannels: OutputStore,
    ): MutableSolver =
        MutablePrologSolver(
            unificator,
            libraries,
            flags,
            staticKb,
            dynamicKb,
            inputChannels,
            outputChannels,
            trustKb = true,
        )

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
            unificator = unificator,
            libraries = libraries,
            flags = flags,
            staticKb = staticKb,
            dynamicKb = dynamicKb.toMutableTheory(),
            operators = operators,
            inputChannels = inputChannels,
            outputChannels = outputChannels,
            customData = customData,
        )

    override fun apply(sideEffect: SideEffect): PrologExecutionContext = super.apply(sideEffect) as PrologExecutionContext

    override fun apply(sideEffects: Iterable<SideEffect>): PrologExecutionContext = super.apply(sideEffects) as PrologExecutionContext

    override fun apply(sideEffects: Sequence<SideEffect>): PrologExecutionContext = super.apply(sideEffects) as PrologExecutionContext

    override fun toString(): String =
        "PrologExecutionContext(" +
            "step=$step, " +
            "depth=$depth, " +
            "substitution=$substitution, " +
            "logicStackTrace=$logicStackTrace, " +
            "goals=$goals, " +
            "rules=$rules, " +
            "primitives=$primitives, " +
            "startTime=$startTime, " +
            "endTime=$endTime, " +
            "maxDuration=$maxDuration, " +
            "unificator=$unificator, " +
            "choicePoints=$choicePoints" +
            ")"
}

/**
 * The goals selected so far in a frame, the newest first: a chain extended at each step, rather than the set of their
 * variables (arg2p: copying the set at each step made long conjunctions quadratic). The variables are only collected
 * when a substitution is trimmed.
 */
class RelevantGoals(
    val goal: Term,
    val previous: RelevantGoals?,
) {
    val goals: Sequence<Term>
        get() = generateSequence(this) { it.previous }.map { it.goal }
}
