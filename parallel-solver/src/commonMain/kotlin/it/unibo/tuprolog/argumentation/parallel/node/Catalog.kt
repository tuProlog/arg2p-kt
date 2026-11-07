package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.AnswerQuery
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.CompactGraph
import it.unibo.tuprolog.argumentation.core.model.mining.activeBranch
import it.unibo.tuprolog.argumentation.core.model.mining.arguments
import it.unibo.tuprolog.argumentation.core.model.mining.compactGraph
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.core.modules.entry.Arg2pModule
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.argumentation.goalOf
import it.unibo.tuprolog.argumentation.haltMessage
import it.unibo.tuprolog.argumentation.parallel.federation.EvaluationStats
import it.unibo.tuprolog.argumentation.parallel.federation.WorkerStats
import it.unibo.tuprolog.argumentation.parallel.message.Ack
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.Attack
import it.unibo.tuprolog.argumentation.parallel.message.AttacksFound
import it.unibo.tuprolog.argumentation.parallel.message.Build
import it.unibo.tuprolog.argumentation.parallel.message.Configure
import it.unibo.tuprolog.argumentation.parallel.message.DirectAttack
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.Explore
import it.unibo.tuprolog.argumentation.parallel.message.Failed
import it.unibo.tuprolog.argumentation.parallel.message.Interest
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.NO_GRAPH
import it.unibo.tuprolog.argumentation.parallel.message.Publish
import it.unibo.tuprolog.argumentation.parallel.message.Rebuild
import it.unibo.tuprolog.argumentation.parallel.message.RequireAnswer
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.RequireSolution
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.message.Response
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.parallel.message.Stop
import it.unibo.tuprolog.argumentation.parallel.message.Subscribe
import it.unibo.tuprolog.argumentation.parallel.partition.Partitioning
import it.unibo.tuprolog.argumentation.parallel.partition.TheoryPartitioner
import it.unibo.tuprolog.argumentation.termOf
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.ParseException
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.channel.OutputChannel
import kotlin.time.TimeMark
import kotlin.time.TimeSource
import it.unibo.tuprolog.core.List as PlList
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * The catalog of a distributed solver: it holds the theory and partitions it among the workers (see
 * [TheoryPartitioner], at most [parts] partitions if given, at most as many as the network can run otherwise), each
 * owning some of the rules, gathers the graph they build (see [it.unibo.tuprolog.argumentation.parallel.message.KbMessage]
 * for the protocol), and solves the goals of its clients on a solver of arg2p, with [flags] and the flags of the theory:
 * 1. each worker is sent its rules and the flags, and tells the premises and the conclusions of its rules; every
 *    worker is then told those of all;
 * 2. the workers build the arguments, sending each one to the workers that build on it and publishing it here: those
 *    of all their rules, or, in query mode (as the standard solver, see the module queryExploration), those the labels
 *    of the statements a goal evaluates depend on, as the semantics declares (directional or connected), explored from
 *    the workers that conclude them;
 * 3. once every Build and Explore is acknowledged, the arguments are all published: the workers find the direct attacks
 *    on their own, as the graph builder does (none with metaConflicts: only the copies attack, added with the graph);
 * 4. the goal is solved as the standard solver solves it, but for the graph: the graph builder of the rules and query
 *    mode of the solver (see [graphModules]) save the arguments and the attacks the workers built, the graph store
 *    extends the attacks to the arguments built on the attacked ones, and the solver derives the other arguments,
 *    changes the graph by its extensions and labels it (every semantics, every graph extension).
 *
 * The catalog knows the shared part of the theory (the flags, the preferences, the conflicts, the Prolog predicates),
 * not the rules, and relays no argument. A goal needing a graph the workers did not build (the whole graph, or in query
 * mode the statements it explores) is solved again from its start once they built it, as is a goal whose solutions are
 * asked for after the solver solved another one; an evaluation of a statement reads the labels of the arguments for it
 * (see answerQuery/4). The graph is built again when the theory changes, when the network loses some of the workers of a
 * graph being built ([Rebuild]), when the flags change ([Configure]), and when a goal needs another graph: the whole
 * one, or in query mode the one of the statement it explores, built for each statement as the standard solver builds
 * it. The evaluations stopped by their clients give up the graph being built for them ([Stop]).
 */
internal class Catalog(
    private val network: Network,
    private val parts: Int? = null,
    private var flags: Flags = Flags.defaults(),
) : Node {
    private val theory = StringBuilder()
    private var changed = true
    private var workers = 0

    // The theory split for as many workers as the network ran when it was split, and how the solver builds the graph:
    // by the catalog alone (a graph not built from rules), and the locality query mode explores (null for the whole
    // graph)
    private var partitioning: Partitioning? = null
    private var partitionedFor: Int? = null
    private var central = false
    private var partial: String? = null

    // The graph built: whether it is (complete, for the theory as it is), its locality (null for the whole graph), and
    // the statements explored so far, or to explore once the workers are ready
    private var built = false
    private var locality: String? = null
    private val explored = mutableListOf<Term>()
    private val pending = mutableListOf<Term>()

    // The construction of the graph: the stage it is at, the replies it still waits for (the subscriptions, the
    // acknowledgements of the Builds and Explores, the attacks of each worker), the premises and conclusions of each
    // worker
    private enum class Stage { READY, SUBSCRIBING, BUILDING, ATTACKING }

    private var stage = Stage.READY
    private var awaited = 0
    private val interests = mutableListOf<Interest>()

    // The arguments published, by their global identifiers, and their supports
    private val arguments = mutableMapOf<String, Term>()

    // The arguments of the graph built, those the workers published, for the tests
    internal val argumentsBuilt: Int get() = arguments.size

    // The arguments of the graph labelled, those derived from the others included, as terms, for the tests
    internal val graphArguments: List<Term> get() = solver.arguments(solver.activeBranch()).map { it.termRepresentation() }
    private val supports = mutableListOf<Pair<String, String>>()

    // The supports bound as the arguments use them (with constraints), not arguments themselves, and the arguments
    private val boundSupports = mutableListOf<Pair<Term, String>>()

    // The direct attacks the workers found
    private val directAttacks = mutableListOf<DirectAttack>()

    // The graph built, as the modules of the solver save it: the arguments and their supports, the attacks; and what
    // the goal solved needs that is not built yet
    private var need: Need? = null

    // What a goal needs the workers to build: the whole graph, or the arguments for a statement in query mode
    private sealed interface Need {
        data object Whole : Need

        data class Explored(
            val statement: Term,
        ) : Need
    }

    // The goals asked for, solved in turn: the jobs waiting, the goals solved by the clients (the last ones), the one
    // whose solutions the solver is computing, and what it printed computing them
    private val jobs = ArrayDeque<Job>()
    private val runs = LinkedHashMap<String, Run>()
    private var current: Run? = null
    private val printed = StringBuilder()

    // The arguments of the graph last reported (see EvaluationStats)
    private var argumentsReported = 0

    private var solver = newSolver()

    // The arguments whose claim unifies with a statement, read as they are (those for it are kept, see
    // sameUpToVariables), and the size of the graph labelled
    private val argumentsFor = Goal("graphStore::argumentUnifiable([S], A)", solver.operators)
    private val size =
        Goal(
            "graphStore::argumentIds(Ids), length(Ids, Arguments), findall(T, graphStore::attackById(T, _, _, _), Ts), " +
                "length(Ts, Attacks)",
            solver.operators,
        )

    // A message handled: an error (a theory that cannot be read, or that its profile refuses) answers the jobs waiting
    // with it (out of memory too, or they would wait for ever), and the theory is read anew for the next ones
    override fun receive(message: KbMessage) {
        try {
            handle(message)
        } catch (error: Throwable) {
            jobs.toList().also { jobs.clear() }.forEach { network.send(it.replyTo, it.failed(error.message ?: error.toString())) }
            // the graph being built is given up (a session of its own, with no worker), the theory read anew
            network.building(0)
            forget()
            stage = Stage.READY
            changed = true
            need = null
            current = null
        }
    }

    private fun handle(message: KbMessage) {
        when (message) {
            Reset -> change { theory.clear() }
            // a graph lost some of its workers: built again now if being built, at the next goal needing it otherwise
            // (its workers are those of a site that left)
            Rebuild -> if (stage != Stage.READY) start((explored + pending).toList(), whole = locality == null) else built = false
            is Load -> change { theory.clear().append(message.theory).append('\n') }
            is Configure ->
                change {
                    flags = Flags.parse(message.flags)
                    solver = newSolver()
                }
            Stop -> if (stage != Stage.READY) stop()
            is Add -> change { theory.append(message.elem.trimEnd().removeSuffix(".")).append(".\n") }
            is RequireEvaluation ->
                try {
                    enqueue(Evaluating(message, Struct.parse(message.elem, solver.operators)))
                } catch (error: ParseException) {
                    val response =
                        EvaluationResponse(emptyList(), emptyList(), emptyList(), "Not a statement: ${message.elem} (${error.message})")
                    network.send(message.replyTo, response)
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
                    network.send(message.replyTo, SolutionResponse(message.run, null, NO_GRAPH, "", "This goal is no longer solved"))
                } else {
                    enqueue(Solving(message.run, run, message.replyTo))
                }
            }
            is RequireAnswer -> enqueue(Answering(message))
            is Subscribe -> {
                interests += Interest(message.worker, message.premises, message.conclusions)
                if (--awaited == 0) {
                    stage = Stage.BUILDING
                    awaited = workers
                    repeat(workers) { network.send(Address.worker(it), Build(interests.toList(), locality)) }
                    explore(pending.toList())
                    pending.clear()
                }
            }
            is Publish -> {
                message.arguments.forEach { argument ->
                    // saved as the argument builder saves them, the last premise first: the graph store gives them
                    // back in the order of the worker
                    argument.supports.asReversed().forEach { supports += it to argument.id }
                    argument.bound.forEach { boundSupports += it to argument.id }
                    arguments[argument.id] = argument.argument
                }
                network.send(Address.worker(message.worker), Ack(-1))
            }
            is Ack -> if (stage == Stage.BUILDING && --awaited == 0) published()
            is AttacksFound -> {
                directAttacks += message.attacks
                if (--awaited == 0) finished()
            }
            // the graph being built given up, the jobs waiting answered with the failure (see receive)
            is Failed -> throw Arg2pException("worker ${message.worker}: ${message.error}")
            else -> Unit
        }
    }

    private fun change(update: () -> Unit) {
        update()
        changed = true
    }

    // The solver of the catalog, with the flags: its theory given before the first job (see prepare)
    private fun newSolver() =
        Arg2pSolverFactory.solverOf(
            Arg2pSolver.of(listOf(Arg2pModule, flags.library()), graphModules(Built())),
            "",
            output = OutputChannel.of { printed.append(it) },
        )

    // The jobs waiting dropped (their clients were told they stopped), and the graph being built for them given up: a
    // session of its own, with no worker, so that nothing more of it reaches the catalog
    private fun stop() {
        jobs.clear()
        need = null
        current = null
        network.building(0)
        forget()
        stage = Stage.READY
    }

    private fun enqueue(job: Job) {
        jobs += job
        work()
    }

    // The jobs waiting done in turn, while no graph is being built: a job needing a graph the workers did not build
    // waits for them to build it (then is done from its start), the theory read anew if it changed
    private fun work() {
        while (stage == Stage.READY && jobs.isNotEmpty()) {
            if (changed) prepare()
            val job = jobs.first()
            // a goal solved from its start: the context marked, as a goal labelling a graph resets it
            if (current !== job.run) solver.solveOnce("context_assert($MARK)")
            val answer =
                try {
                    next(job.run).let { solution ->
                        val wanted = need ?: return@let job.answer(solution)
                        need = null
                        return build(wanted)
                    }
                } catch (error: Arg2pException) {
                    job.failed(error.message ?: error.toString())
                }
            jobs.removeFirst()
            // the statistics of a job labelling or reading a graph, known by its client once answered
            if (job is Evaluating || !solver.holds("context_check($MARK)")) evaluated(job.since)
            network.send(job.replyTo, answer)
        }
    }

    // The next solution of [run]: solved from its start if the solver solved another goal since, the solutions given
    // already left out (and what was printed computing them); null if it has no more, or needs a graph not built yet
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
        val solution = if (solutions.hasNext()) solutions.next() else null
        if (need != null) current = null
        return solution.takeIf { need == null }
    }

    // The solver given the theory, changed: the part of the catalog (see TheoryPartitioner), and how it builds the graph;
    // the graph of the theory before forgotten
    private fun prepare() {
        forget()
        val partitioning =
            try {
                partitioning()
            } catch (refusal: Arg2pException) {
                // a theory its profile refuses: the catalog has it whole, no worker, and refuses it itself when a
                // goal builds its graph (the other goals are solved as the standard solver solves them)
                Partitioning("", emptyList(), converted = false, catalog = theory.toString())
            }
        // (the graph of the last theory dropped first: the solver of a theory changed binds its context to the new program,
        // and would encode every record of it again only for the reset below to forget them)
        solver.solveOnce("context_reset")
        solver.resetStaticKb()
        solver.loadStaticKb(RuleAnnotations.parse(partitioning.catalog, solver.operators, solver.unificator))
        solver.solveOnce("context_reset, parser:::convertAllRules")
        central = !solver.holds("structuredWhole::builder(B), B == ruleGraph")
        partial = if (central) null else locality()
        changed = false
        current = null
    }

    // The theory split for as many workers as the network runs: split again if they changed since
    private fun partitioning(): Partitioning {
        val capacity = parts ?: network.capacity
        val known = partitioning?.takeIf { !changed && partitionedFor == capacity }
        return known ?: TheoryPartitioner.partition(theory.toString(), capacity, flags).also {
            partitioning = it
            partitionedFor = capacity
        }
    }

    // [need] built by the workers, anew: the whole graph, or in query mode the graph of the statement explored
    private fun build(need: Need) =
        when (need) {
            is Need.Explored -> start(listOf(need.statement), whole = false)
            Need.Whole -> start(emptyList(), whole = true)
        }

    // Whether the graph built is the one [need] asks for: the whole graph, or in query mode the one explored for the
    // same statement (a graph for each statement, as the standard solver builds it: kept for the statement asked again)
    private fun met(need: Need) =
        built &&
            when (need) {
                Need.Whole -> locality == null
                is Need.Explored -> locality != null && locality == partial && explored.any { sameUpToVariables(it, need.statement) }
            }

    // The workers loaded with their rules and the flags (the theory split again if they changed), and the locality of
    // the graph chosen (the whole graph if [whole]): [goals] are explored once the workers are ready. A graph built
    // otherwise than from rules (an abstract framework, see graphBuilder) is built by the solver alone.
    private fun start(
        goals: List<Term>,
        whole: Boolean,
    ) {
        val partitioning = partitioning()
        locality = if (whole) null else partial
        val count = if (central) 0 else partitioning.partitions.size
        network.building(count)
        repeat(count) { network.send(Address.worker(it), Load(partitioning.theoryOf(it), flags.toString())) }
        forget()
        workers = count
        if (locality != null) pending += goals
        if (workers == 0) return published()
        stage = Stage.SUBSCRIBING
        awaited = workers
    }

    // The graph built forgotten: its workers, what they told and built
    private fun forget() {
        workers = 0
        built = false
        interests.clear()
        explored.clear()
        pending.clear()
        arguments.clear()
        supports.clear()
        boundSupports.clear()
        directAttacks.clear()
        argumentsReported = 0
    }

    // The part of the graph the labels of the arguments for a statement depend on, as the semantics declares it (see
    // the module structuredPartial): directional or connected in query mode, on a graph built from the rules; null for
    // the whole graph
    private fun locality(): String? =
        solver
            .solveOnce(
                "setting(queryMode), structuredPartial::locality(L), L \\== whole_graph",
            ).takeIf { it.isYes }
            ?.substitution
            ?.getByName("L")
            ?.toString()

    // [goals] explored by the workers whose rules conclude them, each Explore acknowledged
    private fun explore(goals: List<Term>) {
        explored += goals
        interests.forEach { interest ->
            val concluded = ClaimIndex(interest.conclusions, { it })
            val theirs = goals.filter { concluded.anyMatching(it) }
            if (theirs.isNotEmpty()) {
                awaited++
                network.send(Address.worker(interest.worker), Explore(-1, theirs))
            }
        }
    }

    // The arguments all published: the workers find the attacks
    private fun published() {
        directAttacks.clear()
        if (workers == 0 || solver.holds("conflict::argued")) return finished()
        stage = Stage.ATTACKING
        awaited = workers
        repeat(workers) { network.send(Address.worker(it), Attack) }
    }

    // The graph built, kept as the modules of the solver save it (the attacks as attackBuilder saves them): the jobs
    // waiting are done on it
    private fun finished() {
        built = true
        stage = Stage.READY
        work()
    }

    // The statistics of the job just done, waiting since [since]: what each worker built, read from the global
    // identifiers of its arguments (its partition first) and of the arguments its attacks target
    private fun evaluated(since: TimeMark) {
        val published = arguments.keys.groupingBy { it.substringBefore(':').toInt() }.eachCount()
        val found = directAttacks.distinct().groupingBy { it.target.substringBefore(':').toInt() }.eachCount()
        val built = List(workers) { WorkerStats(it, network.siteOf(Address.worker(it)), published[it] ?: 0, found[it] ?: 0) }
        val (graphArguments, graphAttacks) = size.solutions(solver).first().let { it.integer("Arguments") to it.integer("Attacks") }
        val added = arguments.size - argumentsReported
        argumentsReported = arguments.size
        network.evaluated(
            EvaluationStats(
                if (central) "catalog" else locality ?: "whole_graph",
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

    // The graph built, for the modules of the solver: what a goal needs and the workers did not build is asked for
    private inner class Built : BuiltGraph {
        // The identifiers of the arguments in the solver, by their global ones
        private var identifiers = emptyMap<String, Int>()

        override fun arguments(need: Term): Collection<Term>? {
            val wanted = if (need is Struct && need.functor == "explore") Need.Explored(need[0]) else Need.Whole
            if (!met(wanted)) {
                this@Catalog.need = wanted
                return null
            }
            return this@Catalog.arguments.values
        }

        override fun identified(ids: IntArray) {
            identifiers = HashMap<String, Int>(ids.size * 2).also { map -> arguments.keys.forEachIndexed { i, id -> map[id] = ids[i] } }
        }

        override fun supports(): List<Pair<Int, Int>> =
            supports.map { (support, argument) -> identifiers.getValue(support) to identifiers.getValue(argument) }

        override fun boundSupports(): Term =
            PlList.of(boundSupports.map { (support, argument) -> Struct.of("support", support, arguments.getValue(argument)) })

        override fun attacks(): List<Triple<String, Int, Int>> =
            directAttacks.distinct().map { (type, attacker, target) ->
                Triple(type, identifiers.getValue(attacker), identifiers.getValue(target))
            }
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

    // The statements answering a statement, by label, with the arguments for each, as answerQuery/4 answers them (see
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
                                .filter { sameUpToVariables(claimOf(it), labelledStatement) }
                        Response(TermValues.format(labelledStatement), arguments.map { it.toString() }.distinct())
                    }
                }
            val (inStatements, outStatements, undStatements) = responses
            val none = responses.all { it.isEmpty() }
            return EvaluationResponse(inStatements, outStatements, if (none) listOf(Response(request.elem, emptyList())) else undStatements)
        }
    }

    private companion object {
        // The goals of the clients kept, to give their next solutions
        const val MAX_RUNS = 16

        // The fact marking the context of the solver before a goal: gone after it if the goal labelled a graph
        const val MARK = "catalog_mark"
    }
}
