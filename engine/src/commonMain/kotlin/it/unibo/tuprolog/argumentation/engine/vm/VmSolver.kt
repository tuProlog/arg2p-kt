// Forked from 2p-kt's solve 3.0.2 (Apache License 2.0)
package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.channel.InputChannel
import it.unibo.tuprolog.solve.channel.InputStore
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.channel.OutputStore
import it.unibo.tuprolog.solve.currentTimeInstant
import it.unibo.tuprolog.solve.directives.ClausePartition
import it.unibo.tuprolog.solve.directives.partition
import it.unibo.tuprolog.solve.directives.plus
import it.unibo.tuprolog.solve.distinctSolutions
import it.unibo.tuprolog.solve.exception.Warning
import it.unibo.tuprolog.solve.exception.warning.InitializationIssue
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.ShowWildCardVariablesInSolutions
import it.unibo.tuprolog.solve.flags.UniqueSolutions
import it.unibo.tuprolog.solve.getAllOperators
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.toOperatorSet
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator
import it.unibo.tuprolog.utils.buffered

/**
 * A solver running on the [Machine]: the tuProlog `Solver` over a [Program] compiled from the static and the
 * dynamic knowledge base (again when either changes through the solver) and the facade of the libraries. The
 * solvers created for the sub-goals of a resolution (findall/3 and the like, module calls) share its program.
 *
 * What it has of the base of the solvers of 2p-kt (its `AbstractSolver`, forked): the knowledge bases initialised from
 * their directives, the context updated, the options of a query applied to its solutions. 2p-kt cleaned every solution up
 * to the variables of its query, collecting the variables of the query (by sequences over its whole term) and copying the
 * substitution for each solution, which cost as much as the query for each solution of a sub-resolution (a `findall/3`
 * over a long list collected its items in the square of their number). The machine reports the bindings of the variables
 * of the query alone (see `Machine.solve`), so a solution is already as 2p-kt cleaned it up.
 */
@InternalArg2pApi
@Suppress("LeakingThis")
open class VmSolver(
    unificator: Unificator = Unificator.default,
    libraries: Runtime = Runtime.empty(),
    flags: FlagStore = FlagStore.empty(),
    initialStaticKb: Theory = Theory.empty(unificator),
    initialDynamicKb: Theory = MutableTheory.empty(unificator),
    inputChannels: InputStore = InputStore.fromStandard(),
    outputChannels: OutputStore = OutputStore.fromStandard(),
    trustKb: Boolean = false,
) : Solver {
    constructor(
        unificator: Unificator = Unificator.default,
        libraries: Runtime = Runtime.empty(),
        flags: FlagStore = FlagStore.empty(),
        staticKb: Theory = Theory.empty(unificator),
        dynamicKb: Theory = MutableTheory.empty(unificator),
        stdIn: InputChannel<String> = InputChannel.stdIn(),
        stdOut: OutputChannel<String> = OutputChannel.stdOut(),
        stdErr: OutputChannel<String> = OutputChannel.stdErr(),
        warnings: OutputChannel<Warning> = OutputChannel.warn(),
        trustKb: Boolean = false,
    ) : this(
        unificator,
        libraries,
        flags,
        staticKb,
        dynamicKb,
        InputStore.fromStandard(stdIn),
        OutputStore.fromStandard(stdOut, stdErr, warnings),
        trustKb,
    )

    internal var currentContext: PrologExecutionContext

    // The program of the machine: compiled from the knowledge bases when a query comes after they changed
    private var program: Program? = null
    private var outdated = true

    /** The program of the last resolution, if any */
    val currentProgram: Program? get() = program

    /**
     * Whether the theory of the user is visible to the resolutions (not within a module call): not to the goals of the
     * directives of the knowledge bases, run as the solver is made
     */
    var theoryVisible = false

    // The session of the solver, if any: its programs share their symbols with those of the other solvers of the session
    private var session: SolverSession? = null

    init {
        // (setting a unificator indexes the theory anew: not for the one it has)
        val staticKb =
            (
                if (initialStaticKb.unificator ===
                    unificator
                ) {
                    initialStaticKb
                } else {
                    initialStaticKb.setUnificator(unificator)
                }
            ).toImmutableTheory()
        val dynamicKb = initialDynamicKb.setUnificator(unificator).toMutableTheory()
        currentContext =
            PrologExecutionContext(
                ExecutionEnvironment(
                    unificator = unificator,
                    libraries = libraries,
                    flags = flags,
                    staticKb = if (trustKb) staticKb.toImmutableTheory() else Theory.empty(unificator),
                    dynamicKb = if (trustKb) dynamicKb.toMutableTheory() else MutableTheory.empty(unificator),
                    operators = getAllOperators(libraries).toOperatorSet(),
                    inputChannels = inputChannels,
                    outputChannels = outputChannels,
                    startTime = 0,
                ),
            )
        if (!trustKb) {
            initializeKb(staticKb, dynamicKb)
        }
        // (the first query compiles the knowledge bases as the directives left them)
        program = null
        outdated = true
        theoryVisible = true
    }

    /** The solver in [session] from its next resolution (its program made again, in the session) */
    fun join(session: SolverSession) {
        this.session = session
        outdated = true
    }

    /** Runs the resolutions over [program] (that of the solver creating this one), the theory visible or not */
    internal fun share(
        program: Program,
        theoryVisible: Boolean,
    ) {
        this.program = program
        this.session = program.session
        this.outdated = false
        this.theoryVisible = theoryVisible
    }

    internal fun updateContext(operator: PrologExecutionContext.() -> PrologExecutionContext) {
        currentContext = currentContext.operator()
        outdated = true
    }

    private fun resetKb(
        resetStatic: Boolean,
        resetDynamic: Boolean,
    ) {
        updateContext {
            update(
                staticKb = if (resetStatic) Theory.emptyIndexed(unificator) else this.staticKb,
                dynamicKb = if (resetDynamic) MutableTheory.emptyIndexed(unificator) else this.dynamicKb,
            )
        }
    }

    private fun updateContextWith(clausePartition: ClausePartition) {
        updateContext {
            update(
                staticKb = (this.staticKb + clausePartition.staticClauses).toImmutableTheory(),
                dynamicKb = (this.dynamicKb + clausePartition.dynamicClauses).toMutableTheory(),
                operators = operators + clausePartition.operators,
                flags = flags + clausePartition.flagStore,
            )
        }
    }

    internal fun initializeKb(
        staticKb: Theory? = null,
        dynamicKb: Theory? = null,
        appendStatic: Boolean = true,
        appendDynamic: Boolean = true,
    ) {
        val staticSkippable = staticKb.let { it == null || it.directives.none() }
        val dynamicSkippable = dynamicKb.let { it == null || it.directives.none() }
        val merged =
            when {
                staticSkippable && dynamicSkippable -> ClausePartition.of(unificator, staticKb, dynamicKb)
                staticSkippable ->
                    ClausePartition.of(unificator, staticKb) +
                        dynamicKb?.partition(unificator, staticByDefault = false)
                dynamicSkippable -> ClausePartition.of(unificator, dynamicKb) + staticKb?.partition(unificator)
                else -> staticKb?.partition(unificator) + dynamicKb?.partition(unificator, staticByDefault = false)
            }
        resetKb(!appendStatic, !appendDynamic)
        merged?.includes?.map { Struct.of("consult", it) }?.forEach(this::solveInitialGoal)
        merged?.let { updateContextWith(it) }
        merged?.initialGoals?.forEach(this::solveInitialGoal)
    }

    private fun solveInitialGoal(goal: Struct) {
        for (solution in solve(goal)) {
            when (solution) {
                is Solution.No -> {
                    warnings.write(InitializationIssue(goal, null, currentContext))
                }
                is Solution.Halt -> {
                    warnings.write(InitializationIssue(goal, solution.exception, solution.exception.contexts))
                }
                else -> {
                    // does nothing
                }
            }
        }
    }

    final override val unificator: Unificator
        get() = currentContext.unificator

    final override val libraries: Runtime
        get() = currentContext.libraries

    final override val flags: FlagStore
        get() = currentContext.flags

    final override val staticKb: Theory
        get() = currentContext.staticKb

    final override val dynamicKb: Theory
        get() = currentContext.dynamicKb

    final override val inputChannels: InputStore
        get() = currentContext.inputChannels

    final override val outputChannels: OutputStore
        get() = currentContext.outputChannels

    final override val operators: OperatorSet
        get() = currentContext.operators

    private fun program(): Program {
        val current = program
        if (current != null && !outdated) return current
        val built = SharedPrograms.derived(session)
        built.add(staticKb)
        for (clause in dynamicKb.clauses) built.theory.add(clause, dynamic = true)
        program = built
        for (library in libraries.libraries) (library as? ProgramStore)?.bind(built)
        outdated = false
        return built
    }

    /** The solutions of [goal] on the machine, as [options] ask for them */
    final override fun solve(
        goal: Struct,
        options: SolveOptions,
    ): Sequence<Solution> {
        var solutionSequence = solutionsOnMachine(goal, options)
        if (flags[UniqueSolutions] == UniqueSolutions.ON) {
            solutionSequence = solutionSequence.distinctSolutions()
        }
        if (options.limit > 0) {
            solutionSequence = solutionSequence.take(options.limit)
        }
        if (options.isEager) {
            solutionSequence = solutionSequence.buffered()
        }
        return solutionSequence
    }

    private fun solutionsOnMachine(
        goal: Struct,
        options: SolveOptions,
    ): Sequence<Solution> {
        val program = program()
        val environment =
            ExecutionEnvironment(
                unificator = unificator,
                query = goal,
                libraries = libraries,
                flags = flags,
                staticKb = staticKb.toImmutableTheory(),
                dynamicKb = dynamicKb.toMutableTheory(),
                operators = operators,
                inputChannels = inputChannels,
                outputChannels = outputChannels,
                customData = currentContext.customData,
                startTime = currentTimeInstant(),
                maxDuration = options.timeout,
                program = program,
                theoryVisible = theoryVisible,
            )
        currentContext = PrologExecutionContext(environment)
        val machine = Machine(program, environment)
        val deadline = if (options.timeout == Long.MAX_VALUE) Long.MAX_VALUE else environment.startTime + options.timeout
        // (the bindings of the anonymous variables are not reported, nor those of the wildcards if hidden)
        val hideWildcards = flags[ShowWildCardVariablesInSolutions] == ShowWildCardVariablesInSolutions.OFF
        val solutions = machine.solve(goal, deadline, theoryVisible).iterator()
        // (hasNext tells whether a solution may still come, without looking for it, so that a caller asking for one
        // solution does not pay for the next; a No comes when a run finds nothing, none after a solution that left no
        // choice point)
        return Sequence {
            object : Iterator<Solution> {
                private var over = false

                override fun hasNext(): Boolean = !over

                override fun next(): Solution {
                    if (over) throw NoSuchElementException()
                    try {
                        val found = solutions.hasNext()
                        currentContext = currentContext.copy(environment = machine.environment)
                        if (!found) {
                            over = true
                            machine.release()
                            return Solution.no(goal)
                        }
                        val bindings = solutions.next()
                        if (machine.cpTop == 0) {
                            over = true
                            machine.release()
                        }
                        val reported = bindings.filterKeys { !it.isAnonymous && !(hideWildcards && it.isWildcard) }
                        return Solution.yes(goal, Substitution.unifier(reported))
                    } catch (halt: Machine.Halt) {
                        over = true
                        return Solution.halt(goal, halt.exception)
                    }
                }
            }
        }
    }

    override fun copy(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        stdIn: InputChannel<String>,
        stdOut: OutputChannel<String>,
        stdErr: OutputChannel<String>,
        warnings: OutputChannel<Warning>,
    ): VmSolver = VmSolver(unificator, libraries, flags, staticKb, dynamicKb, stdIn, stdOut, stdErr, warnings)

    override fun clone(): VmSolver = copy()

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as VmSolver

        if (currentContext != other.currentContext) return false

        return true
    }

    override fun hashCode(): Int = currentContext.hashCode()

    override fun toString(): String =
        "${this::class.simpleName}(" +
            "libraries=${libraries.aliases}, " +
            "staticKb=${staticKb.clauses.toList()}, " +
            "dynamicKb=${dynamicKb.clauses.toList()}, " +
            "operators=$operators" +
            "flags=$flags, " +
            "inputChannels=$inputChannels, " +
            "outputChannels=$outputChannels, " +
            ")"
}
