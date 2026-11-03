// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.fsm.Bindings
import it.unibo.tuprolog.argumentation.core.engine.fsm.TheoryIndexes
import it.unibo.tuprolog.argumentation.core.engine.fsm.collectVariables
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
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
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.sideffects.SideEffect
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
 * The fields of [it.unibo.tuprolog.solve.ExecutionContext] but [procedure] and [substitution] are read from the
 * [environment] of the frame (arg2p, see [ExecutionEnvironment]), and [substitution] is the unifier of the [bindings]
 * of the frame, built when read (see [Bindings]); the ones specific to the prolog engine are:
 * - [query]/[goals]: the original query, and the cursor over the goals still to be proven in this frame.
 * - [rules]/[primitives]: the still-untried candidate clauses/primitive responses for the current goal, i.e. the
 *   frame's own view of the [choicePoints] entry recorded for it (if any).
 * - [choicePoints]: the head of the choice-point queue as seen from this frame -- see [ChoicePointContext].
 * - [parent]/[depth]: the enclosing frame and this frame's distance from the root (`depth == 0`, [isRoot]).
 * - [step]: a monotonically increasing counter of state-machine transitions, used to keep [PrologSolver]'s
 *   `currentContext` in sync with resolution progress and to detect [it.unibo.tuprolog.solve.exception.TimeOutException]s.
 * - [trimmedSize]: the size of the [substitution] when it was last trimmed of the variables local to a finished
 *   sub-goal, popping back to [parent] (see [isVariableInteresting]).
 * - [inlined]: whether this frame solves the goal of an [Inlining], against libraries of its own; [scoped]: whether
 *   this frame or one above does (so that its libraries are not the ones of the whole resolution).
 * - [theoryVisible]: whether the goals of this frame see the theory of the user (its static and dynamic knowledge
 *   base): they do outside module calls and in `user::` calls; the modules see it only through `user::`.
 * - [theoryIndexes]: the indexes of the immutable theories, kept by the solver and shared with the solvers the
 *   resolution creates (see [TheoryIndexes]).
 *
 * @throws IllegalArgumentException if [depth] and [parent] are inconsistent (`depth == 0` iff `parent == null`).
 */
data class PrologExecutionContext(
    val environment: ExecutionEnvironment,
    override val procedure: Struct? = null,
    val bindings: Bindings = Bindings.EMPTY,
    val goals: Cursor<out Term> = Cursor.empty(),
    val rules: Cursor<out Rule> = Cursor.empty(),
    val primitives: Cursor<out Solve.Response> = Cursor.empty(),
    val choicePoints: ChoicePointContext? = null,
    val parent: PrologExecutionContext? = null,
    val depth: Int = 0,
    // (A var: the transitions that keep the frame as it is advance it in place, see advanced)
    var step: Long = 0,
    val trimmedSize: Int = 0,
    val inlined: Boolean = false,
    val scoped: Boolean = false,
    val theoryVisible: Boolean = true,
) : ExecutionContext {
    init {
        require((depth == 0 && parent == null) || (depth > 0 && parent != null))
    }

    override val substitution: Substitution.Unifier
        get() = bindings.unifier

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

    val query: Struct
        get() = environment.query

    val theoryIndexes: TheoryIndexes
        get() = environment.theoryIndexes

    /** Whether this is the outermost frame of a resolution (i.e. it has no [parent]). */
    val isRoot: Boolean
        get() = depth == 0

    /** Whether [choicePoints] (or any of its ancestors) still has an alternative to backtrack into. */
    val hasOpenAlternatives: Boolean
        get() = choicePoints?.hasOpenAlternatives ?: false

    /**
     * This frame and all its ancestors, from here up to (and including) the root, in that order (arg2p: built when
     * needed, rather than for each of the contexts, one or more per resolution step).
     */
    val pathToRoot: Sequence<PrologExecutionContext>
        get() = generateSequence(this) { it.parent }

    /** The root of this frame (arg2p: by a loop, where [pathToRoot] built a sequence) */
    val root: PrologExecutionContext
        get() {
            var frame = this
            while (true) frame = frame.parent ?: return frame
        }

    // arg2p: the values computed when first needed are kept in fields rather than lazy delegates, which a copy of the
    // context (made at almost every step) allocated anew, with their initializers, for each of them
    private var currentGoalValue: Term? = null
    private var currentGoalComputed = false
    private var interestingVariablesValue: KtSet<Var>? = null

    /** The current [goals] entry with [substitution] already applied, or `null` if there is none left to prove. */
    val currentGoal: Term?
        get() {
            if (!currentGoalComputed) {
                currentGoalValue = if (goals.isOver) null else goals.current?.let { bindings.resolved(it) }
                currentGoalComputed = true
            }
            return currentGoalValue
        }

    // A set rather than a sequence: membership is checked for every binding of the substitution whenever a frame is
    // left, and a sequence is scanned at each check (arg2p: the scans were quadratic in deep resolutions)
    private val interestingVariables: KtSet<Var>
        get() = interestingVariablesValue ?: collectInterestingVariables().also { interestingVariablesValue = it }

    private fun collectInterestingVariables(): KtSet<Var> =
        HashSet<Var>().also { variables ->
            query.collectVariables(variables)
            var context: PrologExecutionContext? = this
            while (context != null) {
                // arg2p: the goals left to solve, not only the current one (as in tuProlog): a variable bound by a
                // goal before and used by one after would otherwise lose its binding when a sub-goal ends
                var left = context.goals
                while (!left.isOver) {
                    left.current?.collectVariables(variables)
                    left = left.next
                }
                context = context.parent
            }
        }

    /**
     * Whether [variable] is still relevant to this frame or any of its ancestors (i.e. occurs in [relevantGoals],
     * in the goals left of this frame or of an ancestor, or in the [query]). Used when popping a finished
     * frame back to its [parent] (in [it.unibo.tuprolog.argumentation.core.engine.fsm.StateGoalSelection]) to filter the
     * [substitution] handed up, discarding bindings for variables local to the finished sub-goal.
     */
    fun isVariableInteresting(variable: Var) = variable in interestingVariables

    // (arg2p: built at each request, which only the errors make, rather than kept in a field of every frame)
    override val logicStackTrace: KtList<Struct>
        get() = pathToRoot.map { it.procedure ?: Struct.of("?-", query) }.toList()

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
        ).also {
            it.shareTheoryIndexes(theoryIndexes)
            it.theoryVisible = theoryVisible
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
        MutablePrologSolver(
            unificator,
            libraries,
            flags,
            staticKb,
            dynamicKb,
            inputChannels,
            outputChannels,
            trustKb = true,
        ).also {
            it.shareTheoryIndexes(theoryIndexes)
            it.theoryVisible = theoryVisible
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

    /** This frame with the given values in its [environment] (arg2p: as a copy of the frame with them, before) */
    fun withEnvironment(
        unificator: Unificator = this.unificator,
        libraries: Runtime = this.libraries,
        flags: FlagStore = this.flags,
        staticKb: Theory = this.staticKb,
        dynamicKb: MutableTheory = this.dynamicKb,
        operators: OperatorSet = this.operators,
        inputChannels: InputStore = this.inputChannels,
        outputChannels: OutputStore = this.outputChannels,
        customData: CustomDataStore = this.customData,
    ): PrologExecutionContext =
        copy(
            environment =
                environment.copy(
                    unificator = unificator,
                    libraries = libraries,
                    flags = flags,
                    staticKb = staticKb,
                    dynamicKb = dynamicKb,
                    operators = operators,
                    inputChannels = inputChannels,
                    outputChannels = outputChannels,
                    customData = customData,
                ),
        )

    override fun apply(sideEffect: SideEffect): PrologExecutionContext = super.apply(sideEffect) as PrologExecutionContext

    override fun apply(sideEffects: Iterable<SideEffect>): PrologExecutionContext = super.apply(sideEffects) as PrologExecutionContext

    override fun apply(sideEffects: Sequence<SideEffect>): PrologExecutionContext = super.apply(sideEffects) as PrologExecutionContext

    override fun toString(): String =
        "PrologExecutionContext(" +
            "step=$step, " +
            "depth=$depth, " +
            "bindings=$bindings, " +
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
