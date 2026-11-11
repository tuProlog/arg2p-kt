package it.unibo.tuprolog.argumentation.parallel.consumer

import it.unibo.tuprolog.argumentation.AnswerQuery
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.TermValues
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libraries.LibraryRegistry
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.TheoryNotation
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.activeBranch
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.arguments
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.sharing.GraphSharing
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.sharing.graphKey
import it.unibo.tuprolog.argumentation.core.model.CompactGraph
import it.unibo.tuprolog.argumentation.core.model.compactGraph
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.store.Record
import it.unibo.tuprolog.argumentation.engine.vm.program.SolverSession
import it.unibo.tuprolog.argumentation.goalOf
import it.unibo.tuprolog.argumentation.haltMessage
import it.unibo.tuprolog.argumentation.parallel.EvaluationStats
import it.unibo.tuprolog.argumentation.parallel.WorkerStats
import it.unibo.tuprolog.argumentation.parallel.host.locked
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.Configure
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.NO_GRAPH
import it.unibo.tuprolog.argumentation.parallel.message.Rebuild
import it.unibo.tuprolog.argumentation.parallel.message.RequireAnswer
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.RequireSolution
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.message.Response
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.parallel.message.Stop
import it.unibo.tuprolog.argumentation.parallel.store.SharedGraph
import it.unibo.tuprolog.argumentation.termOf
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.ParseException
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.theory.Theory
import kotlin.time.TimeMark
import kotlin.time.TimeSource
import it.unibo.tuprolog.core.List as PlList
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * The consumer that answers the clients of a node: it solves their goals as the standard solver does, with [flags] and
 * the flags of the theory, the graph of each built with other consumers when the theory splits (see [TheoryPartitioner]:
 * into at most [parts] partitions if given, as many as the consumers the environment can run otherwise). It is then one
 * of the consumers building the graph (see [SharedGraph]), the consumer 0, with the rules of the first partition, the
 * others started when the graph begins (see [GraphSharing.begin]). Every consumer runs the pipeline of the standard solver
 * on its rules, up to the labelling; the labelling of a goal is done by the solver it was asked of, on the graph of all of
 * them.
 *
 * The whole graph is built once for the theory, and in query mode a graph for each statement explored, as the standard
 * solver builds them: a goal reading a graph built already reads it again from what every consumer built, no other
 * consumer involved. A graph is built anew when the theory changes, when the flags change ([Configure]), when a consumer
 * failed building it, and when a host running some of its consumers left ([Rebuild]). The evaluations stopped by their
 * clients give up the graph being built for them ([Stop]).
 */
internal class ClientSolver(
    private val environment: SolverEnvironment,
    private val parts: Int? = null,
    private var flags: Flags = Flags.defaults(),
) {
    private val theory = StringBuilder()
    private var changed = true

    // The theory split for as many consumers as the environment ran when it was split, and how the graph is built: by this
    // solver alone (a graph not built from rules, a theory not split), and the locality query mode explores (null for
    // the whole graph)
    private var partitioning: Partitioning? = null
    private var partitionedFor: Int? = null
    private var alone = true
    private var partial: String? = null

    // The goals asked for, solved in turn: the jobs waiting, the goals solved by the clients (the last ones), the one
    // whose solutions the solver is computing, and what it printed computing them
    private val jobs = ArrayDeque<Job>()
    private val runs = LinkedHashMap<String, Run>()
    private var current: Run? = null
    private val printed = StringBuilder()

    private var solver = newSolver()

    // The session of the solvers of the graphs of the theory in this process, this one's and the consumers it runs here:
    // one for each theory, its symbols dropped with it; and the graphs built
    private var solvers: SolverSession? = null
    private val graphs = Graphs()

    // The arguments of the graph labelled, those derived from the others included, as terms, for the tests
    internal val graphArguments: List<Term> get() = solver.arguments(solver.activeBranch()).map { it.termRepresentation() }

    // The arguments standing for a statement answered (see statementAcceptance::answering/2), and the size of the graph
    // labelled
    private val argumentsFor = Goal("statementAcceptance::answering(S, A)", solver.operators)
    private val size =
        Goal(
            "store::argumentIds(Ids), length(Ids, Arguments), findall(T, store::attackById(T, _, _, _), Ts), " +
                "length(Ts, Attacks)",
            solver.operators,
        )

    // A message handled: an error (a theory that cannot be read, or that its profile refuses) answers the jobs waiting
    // with it (out of memory too, or they would wait for ever), and the theory is read anew for the next ones
    fun receive(message: KbMessage) {
        try {
            handle(message)
        } catch (error: Throwable) {
            jobs.toList().also { jobs.clear() }.forEach { environment.reply(it.replyTo, it.failed(error.message ?: error.toString())) }
            graphs.forget()
            changed = true
            current = null
        }
    }

    private fun handle(message: KbMessage) {
        when (message) {
            Reset -> change { theory.clear() }
            // a host running consumers of a graph left: the graph is built anew by the next goal needing it
            Rebuild -> graphs.forget()
            is Load -> change { theory.clear().append(message.theory).append('\n') }
            is Configure ->
                change {
                    flags = Flags.parse(message.flags)
                    solver = newSolver()
                }
            // (the goal under way interrupted by the node, the graph being built for it given up)
            Stop -> {
                jobs.clear()
                current = null
                graphs.forget()
                // (prepared again: a stop that came while the theory was prepared left it as if it could not be shared)
                changed = true
            }
            is Add -> change { theory.append(message.elem.trimEnd().removeSuffix(".")).append(".\n") }
            is RequireEvaluation ->
                try {
                    enqueue(Evaluating(message, Struct.parse(message.elem, solver.operators)))
                } catch (error: ParseException) {
                    val response =
                        EvaluationResponse(emptyList(), emptyList(), emptyList(), "Not a statement: ${message.elem} (${error.message})")
                    environment.reply(message.replyTo, response)
                }
            is RequireSolution -> {
                val run =
                    if (message.goal == null) {
                        // asked for again: the last to be evicted
                        runs.remove(message.run)?.also { runs[message.run] = it }
                    } else {
                        Run { goalOf(message.goal, it) }.also {
                            runs[message.run] = it
                            if (runs.size > MAX_RUNS) runs.remove(runs.keys.first())
                        }
                    }
                if (run == null) {
                    environment.reply(message.replyTo, SolutionResponse(message.run, null, NO_GRAPH, "", "This goal is no longer solved"))
                } else {
                    enqueue(Solving(message.run, run, message.replyTo))
                }
            }
            is RequireAnswer -> enqueue(Answering(message))
            else -> Unit
        }
    }

    private fun change(update: () -> Unit) {
        update()
        changed = true
    }

    // The solver, with the flags: its theory given before the first job (see prepare)
    private fun newSolver() =
        Arg2pSolverFactory.solverOf(Arg2pSolver.default(listOf(flags.library())), "", output = OutputChannel.of { printed.append(it) })

    private fun enqueue(job: Job) {
        jobs += job
        work()
    }

    // The jobs waiting done in turn, the theory read anew if it changed. A job whose graph a host running some of its
    // consumers left while it was built is done again, from its start, with the hosts left (a few times at most).
    private fun work() {
        var again = 0
        while (jobs.isNotEmpty()) {
            if (changed) prepare()
            val job = jobs.first()
            // a goal solved from its start: the context marked, as a goal labelling a graph resets it
            if (current !== job.run) solver.solveOnce("contextAssert($MARK)")
            var solved = false
            val answer =
                try {
                    job.answer(next(job.run).also { solved = it != null })
                } catch (error: Arg2pException) {
                    job.failed(error.message ?: error.toString())
                }
            if (graphs.last?.lost == true && again++ < AGAIN) {
                // (the solution of the graph lost is none the client was given)
                if (solved) job.run.given--
                graphs.forget()
                current = null
                continue
            }
            again = 0
            jobs.removeFirst()
            // the statistics of a job labelling or reading a graph, known by its client once answered
            if (job is Evaluating || !solver.holds("contextCheck($MARK)")) evaluated(job.since)
            environment.reply(job.replyTo, answer)
        }
    }

    // The next solution of [run]: solved from its start if the solver solved another goal since, the solutions given
    // already left out (and what was printed computing them); null if it has no more
    private fun next(run: Run): LogicSolution? {
        if (current !== run) {
            val goal = run.goal(solver)
            current = run
            printed.clear()
            run.solutions = solver.solve(goal).iterator()
            repeat(run.given) { pull(run) ?: return null }
        }
        return pull(run)?.also { run.given++ }
    }

    private fun pull(run: Run): LogicSolution? {
        val solutions = run.solutions ?: return null
        return if (solutions.hasNext()) solutions.next() else null
    }

    // The solver given the theory, changed: the rules of its partition and the shared part of the theory if it splits
    // (see TheoryPartitioner), the whole theory otherwise; the graphs of the theory before forgotten
    private fun prepare() {
        graphs.forget()
        val partitioning =
            try {
                partitioning()
            } catch (refusal: Arg2pException) {
                // a theory its profile refuses: whole here, refused when a goal builds its graph (the other goals are
                // solved as the standard solver solves them)
                null
            }
        // (the graph of the last theory dropped first: the solver of a theory changed binds its context to the new program,
        // and would encode every record of it again only for the reset below to forget them)
        solver.solveOnce("contextReset")
        solvers = SolverSession().also { SolverSession.join(solver, it) }
        val split = partitioning?.takeIf { it.partitions.size > 1 }
        if (split == null) loadTheory() else load(split.theoryOf(0))
        // a graph not built from rules (an abstract framework, see graphBuilder) is built here alone, of the whole theory
        alone = split == null || !solver.holds("pipeline::builder(B), B == ruleGraph")
        if (alone && split != null) loadTheory()
        partial = if (alone) null else locality()
        solvers!![GraphSharing.KEY] = if (alone) null else graphs
        changed = false
        current = null
    }

    // The theory of the clients, read with the notation of its languages (see TheoryNotation)
    private fun loadTheory() =
        load(TheoryNotation.read(theory.toString(), listOf(flags.library().baseContent.clauses), LibraryRegistry.all, solver.unificator))

    // The part [text] of the theory, written by the partitioner of the theory read (see TheoryPartitioner): with all the operators
    private fun load(text: String) = load(RuleAnnotations.parse(text, solver.operators, solver.unificator))

    private fun load(clauses: Theory) {
        solver.resetStaticKb()
        solver.loadStaticKb(clauses)
        solver.solveOnce("contextReset")
    }

    // The theory split for as many consumers as the environment runs: split again if they changed since
    private fun partitioning(): Partitioning {
        val capacity = parts ?: environment.capacity
        val known = partitioning?.takeIf { !changed && partitionedFor == capacity }
        return known ?: TheoryPartitioner.partition(theory.toString(), capacity, flags).also {
            partitioning = it
            partitionedFor = capacity
        }
    }

    // The part of the graph the labels of the arguments for a statement depend on, as the semantics declares it (see
    // pipeline::locality/1): directional or connected in query mode, on a graph built from the rules; null for
    // the whole graph
    private fun locality(): String? =
        solver
            .solveOnce(
                "pipeline::locality(L), L \\== wholeGraph",
            ).takeIf { it.isYes }
            ?.substitution
            ?.getByName("L")
            ?.toString()

    // The statistics of the job just done, waiting since [since]: what each consumer of the graph built
    private fun evaluated(since: TimeMark) {
        val graph = graphs.last
        val built =
            graph
                ?.let { shared ->
                    val counts = environment.published(shared)
                    shared.sites.mapIndexed { part, site -> WorkerStats(part, site, counts[part]?.first ?: 0, counts[part]?.second ?: 0) }
                }.orEmpty()
        val (graphArguments, graphAttacks) = size.solutions(solver).first().let { it.integer("Arguments") to it.integer("Attacks") }
        val total = built.sumOf { it.arguments }
        val added = if (graph == null) 0 else (total - graph.reported).also { graph.reported = total }
        environment.evaluated(
            EvaluationStats(
                if (alone) {
                    "alone"
                } else if (graph?.exploring == true) {
                    partial ?: "wholeGraph"
                } else {
                    "wholeGraph"
                },
                graphArguments,
                graphAttacks,
                added,
                built,
                since.elapsedNow().inWholeMicroseconds / 1000.0,
            ),
        )
    }

    // The graph of the last solution, none for a solution that labels none
    private fun mined(): CompactGraph =
        try {
            solver.compactGraph()
        } catch (_: Exception) {
            NO_GRAPH
        }

    /**
     * The graphs this solver builds with others, as the sharing of its session: the whole graph, and in query mode one for
     * each statement explored (the last ones), chosen when a graph begins (see [GraphSharing.begin]) and started then if
     * new, its consumers here (sharing the session) going to theirs.
     */
    private inner class Graphs : GraphSharing {
        private var whole: SharedGraph? = null
        private val queries = ArrayList<Pair<Term, SharedGraph>>()

        // the graph of each consumer run here, by its solver, and the one this solver builds
        private val ofConsumer = HashMap<Any, SharedGraph>()
        private var mine: SharedGraph? = null

        /** The graph of the last goal, if shared */
        var last: SharedGraph? = null
            private set

        fun forget() {
            if (whole != null || queries.isNotEmpty()) environment.forgetGraphs()
            whole = null
            queries.clear()
            ofConsumer.clear()
            mine = null
            last = null
        }

        // (the graph of a consumer run here, or the one this solver builds)
        private fun of(call: NativeCall): SharedGraph? {
            val key = call.graphKey()
            return locked(ofConsumer) { ofConsumer[key] } ?: mine?.takeIf { key == solver.graphKey() }
        }

        override fun begin(
            call: NativeCall,
            statement: Term?,
        ) {
            // (a consumer run here beginning its graph: nothing to choose)
            if (call.graphKey() != solver.graphKey()) return
            val split = partitioning ?: return
            val graph =
                if (statement == null) {
                    whole?.takeUnless { it.failed } ?: started(split, exploring = false, wholeGoal()).also { whole = it }
                } else {
                    queries.removeAll { it.second.failed }
                    queries.firstOrNull { sameUpToVariables(it.first, statement) }?.second
                        ?: started(split, exploring = true, queryGoal(statement)).also {
                            queries += statement to it
                            if (queries.size > MAX_RUNS) queries.removeAt(0)
                        }
                }
            if (!graph.joined(call)) graph.assign(call, 0, if (graph.exploring) null else split.interests[0])
            mine = graph
            last = graph
        }

        // A graph of the consumers of [split], this solver the consumer 0, the others started on the hosts the environment
        // places them on, or here on threads of their own, each solving [goal] once its rules are converted
        private fun started(
            split: Partitioning,
            exploring: Boolean,
            goal: String,
        ): SharedGraph {
            val count = split.partitions.size
            val text = "contextReset, $goal"
            val starts =
                (1 until count).associateWith { part ->
                    ConsumerStart(split.theoryOf(part), flags.toString(), text, if (exploring) null else split.interests[part], exploring)
                }
            val made = environment.newGraph(count, exploring, starts)
            val graph = made.graph
            val run = environment.apart()
            for ((part, start) in made.here) {
                if (run == null) {
                    graph.fail("the node cannot run the solver of part $part")
                    continue
                }
                run {
                    start
                        .run { worker ->
                            SolverSession.join(worker, solvers!!)
                            graph.assign(worker, part, start.interest)
                            worker.graphKey()?.let { key -> locked(ofConsumer) { ofConsumer[key] = graph } }
                        }?.let { graph.fail("the solver of part $part: $it") }
                }
            }
            return graph
        }

        private fun wholeGoal() = "pipeline::graph(whole, wholeGraph), pipeline::modifyGraph"

        private fun queryGoal(statement: Term) = "pipeline::locality(L), pipeline::graph($statement, L), pipeline::modifyGraph"

        override fun share(
            call: NativeCall,
            tag: String,
            nothingNew: Boolean,
        ) = of(call)?.share(call, tag, nothingNew) ?: Unit

        override fun explore(call: NativeCall): Pair<List<Term>, List<Term>>? = of(call)?.explore(call)

        override fun settle(call: NativeCall) = of(call)?.settle(call) ?: Unit

        override fun step(
            call: NativeCall,
            ownAttacks: Boolean,
        ) = of(call)?.step(call, ownAttacks) ?: Unit

        override fun own(call: NativeCall): IntArray? = of(call)?.own(call)

        override fun sync(call: NativeCall): IntArray = of(call)?.sync(call) ?: IntArray(0)

        override fun removed(
            call: NativeCall,
            facts: List<Record>,
        ) = of(call)?.removed(call, facts) ?: Unit

        override fun saved(
            call: NativeCall,
            fact: Record,
        ) = of(call)?.saved(call, fact) ?: Unit
    }

    // A goal solved for a client, read by [read] (with the operators of the theory) the first time it is solved: the
    // solutions given so far and the characters printed computing them, and the solutions of the solver while it
    // solves this goal
    private class Run(
        private val read: (OperatorSet) -> Struct,
    ) {
        private var goal: Struct? = null
        var given = 0
        var printed = 0
        var solutions: Iterator<LogicSolution>? = null

        fun goal(solver: Solver): Struct = goal ?: read(solver.operators).also { goal = it }
    }

    // A job: the next solution of [run], answered to [replyTo], or the error that kept it from one; it waits [since]
    private abstract inner class Job(
        val replyTo: String,
    ) {
        abstract val run: Run

        val since = TimeSource.Monotonic.markNow()

        abstract fun answer(solution: LogicSolution?): KbMessage

        abstract fun failed(error: String): KbMessage
    }

    // The next solution of the goal of the client, with the graph of its resolution and what the solver printed: the
    // one after it is not computed until the client asks for it
    private inner class Solving(
        private val id: String,
        override val run: Run,
        replyTo: String,
    ) : Job(replyTo) {
        override fun answer(solution: LogicSolution?): KbMessage {
            val graph = solution?.let { mined() } ?: NO_GRAPH
            val output = printed.substring(minOf(run.printed, printed.length))
            run.printed += output.length
            return SolutionResponse(id, solution, graph, output)
        }

        override fun failed(error: String) = SolutionResponse(id, null, NO_GRAPH, "", error)
    }

    // The statements for a query by label, as the standard solver answers them: every answer at once
    private inner class Answering(
        request: RequireAnswer,
    ) : Job(request.replyTo) {
        private val query by lazy { AnswerQuery(termOf(request.query, solver.operators)) }

        override val run = Run { query.allGoal }

        override fun answer(solution: LogicSolution?): KbMessage =
            if (solution?.isHalt == true) {
                failed(solution.haltMessage())
            } else {
                // the graph may hold more than the query depends on (that of the goals before): a labelling of what
                // it does not depend on answers the same
                AnswerResponse(solution?.takeIf { it.isYes }?.let { query.answers(it).distinct() }.orEmpty())
            }

        override fun failed(error: String) = AnswerResponse(null, error)
    }

    // The statements answering a statement, by label, with the arguments for each, as solve/4 answers them (see
    // the flag answerMode)
    private inner class Evaluating(
        private val request: RequireEvaluation,
        private val statement: Term,
    ) : Job(request.replyTo) {
        private val query by lazy { AnswerQuery(statement) }

        override val run = Run { query.goal }

        override fun answer(solution: LogicSolution?): KbMessage {
            if (solution == null) return EvaluationResponse(emptyList(), emptyList(), emptyList())
            if (solution.isHalt) return failed(solution.haltMessage())
            return labels(query.statements(solution).map { PlList.of(it) })
        }

        override fun failed(error: String) = EvaluationResponse(emptyList(), emptyList(), emptyList(), error)

        // The statements answering the one asked, labelled in, out and und ([In, Out, Und] lists), each with the
        // arguments for it; a statement no argument concludes is UND, for no argument
        private fun labels(labelled: List<Term>): EvaluationResponse {
            val responses =
                labelled.map { statements ->
                    statements.castToList().toList().map { labelledStatement ->
                        val arguments =
                            argumentsFor
                                .solutions(solver, "S" to labelledStatement)
                                .map { it.getValue("A") }
                        Response(TermValues.format(labelledStatement), arguments.map { it.toString() }.distinct())
                    }
                }
            val (inStatements, outStatements, undStatements) = responses
            val none = responses.all { it.isEmpty() }
            return EvaluationResponse(inStatements, outStatements, if (none) listOf(Response(request.elem, emptyList())) else undStatements)
        }
    }

    private companion object {
        // The goals of the clients kept, to give their next solutions, and the graphs of the statements explored
        const val MAX_RUNS = 16

        // The fact marking the context of the solver before a goal: gone after it if the goal labelled a graph
        const val MARK = "solver_mark"

        // The times a job is done again, its graph lost by a host leaving
        const val AGAIN = 3
    }
}
