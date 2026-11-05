package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.AbstractSolver
import it.unibo.tuprolog.argumentation.core.engine.ExecutionEnvironment
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.runtime.cache.Cache
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.channel.InputChannel
import it.unibo.tuprolog.solve.channel.InputStore
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.channel.OutputStore
import it.unibo.tuprolog.solve.currentTimeInstant
import it.unibo.tuprolog.solve.exception.Warning
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.ShowWildCardVariablesInSolutions
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * A solver running on the [Machine] (spike): the tuProlog `Solver` over a [Program] compiled from the static and the
 * dynamic knowledge base (again when either changes through the solver) and the facade of the libraries. The
 * solvers created for the sub-goals of a resolution (findall/3 and the like, module calls) share its program.
 */
internal open class VmSolver : AbstractSolver<PrologExecutionContext> {
    constructor(
        unificator: Unificator = Unificator.default,
        libraries: Runtime = Runtime.empty(),
        flags: FlagStore = FlagStore.empty(),
        initialStaticKb: Theory = Theory.empty(unificator),
        initialDynamicKb: Theory = MutableTheory.empty(unificator),
        inputChannels: InputStore = InputStore.fromStandard(),
        outputChannels: OutputStore = OutputStore.fromStandard(),
        trustKb: Boolean = false,
    ) : super(unificator, libraries, flags, initialStaticKb, initialDynamicKb, inputChannels, outputChannels, trustKb)

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

    override lateinit var currentContext: PrologExecutionContext

    // The program of the machine: compiled from the knowledge bases when a query comes after they changed
    private var program: Program? = null
    private var outdated = true

    /** Whether the theory of the user is visible to the resolutions (not within a module call) */
    var theoryVisible = true

    /** Runs the resolutions over [program] (that of the solver creating this one), the theory visible or not */
    fun share(
        program: Any?,
        theoryVisible: Boolean,
    ) {
        this.program = program as Program
        this.outdated = false
        this.theoryVisible = theoryVisible
    }

    final override fun initializeContext(
        unificator: Unificator,
        libraries: Runtime,
        flags: FlagStore,
        staticKb: Theory,
        dynamicKb: Theory,
        operators: OperatorSet,
        inputChannels: InputStore,
        outputChannels: OutputStore,
        trustKb: Boolean,
    ) = PrologExecutionContext(
        ExecutionEnvironment(
            unificator = unificator,
            libraries = libraries,
            flags = flags,
            staticKb = if (trustKb) staticKb.toImmutableTheory() else Theory.empty(unificator),
            dynamicKb = if (trustKb) dynamicKb.toMutableTheory() else MutableTheory.empty(unificator),
            operators = operators,
            inputChannels = inputChannels,
            outputChannels = outputChannels,
            startTime = 0,
        ),
    )

    override fun onContextUpdated() {
        outdated = true
    }

    private fun program(): Program {
        val current = program
        if (current != null && !outdated) return current
        val built = SharedPrograms.derived()
        built.add(staticKb)
        for (clause in dynamicKb.clauses) built.theory.add(clause, dynamic = true)
        program = built
        // (the stores of arg2p keep their records in the words of the program)
        Context.of(libraries)?.bind(built)
        Cache.of(libraries)?.bind(built)
        outdated = false
        return built
    }

    final override fun solveImpl(
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
                vm = program,
                theoryVisible = theoryVisible,
            )
        currentContext = PrologExecutionContext(environment)
        val machine = Machine(program)
        machine.facade(environment)
        val deadline = if (options.timeout == Long.MAX_VALUE) Long.MAX_VALUE else environment.startTime + options.timeout
        // (as the engine cleans a solution up: the bindings of the anonymous variables are not reported, nor those of
        // the wildcards if hidden)
        val hideWildcards = flags[ShowWildCardVariablesInSolutions] == ShowWildCardVariablesInSolutions.OFF
        val solutions = machine.solve(goal, deadline, theoryVisible).iterator()
        // (as the engine: hasNext tells whether a solution may still come, without looking for it, so that a caller
        // asking for one solution does not pay for the next; a No comes when a run finds nothing, none after a
        // solution that left no choice point)
        return Sequence {
            object : Iterator<Solution> {
                private var over = false

                override fun hasNext(): Boolean = !over

                override fun next(): Solution {
                    if (over) throw NoSuchElementException()
                    try {
                        val found = solutions.hasNext()
                        machine.environment?.let { currentContext = currentContext.copy(environment = it) }
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
}
