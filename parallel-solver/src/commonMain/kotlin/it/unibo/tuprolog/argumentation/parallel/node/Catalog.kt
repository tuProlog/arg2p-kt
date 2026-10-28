package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.AnswerQuery
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.SyntaxProblem
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.mining.activeBranch
import it.unibo.tuprolog.argumentation.core.model.mining.arguments
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.argumentation.core.modules.entry.Arg2pModule
import it.unibo.tuprolog.argumentation.parallel.federation.EvaluationStats
import it.unibo.tuprolog.argumentation.parallel.federation.WorkerStats
import it.unibo.tuprolog.argumentation.parallel.message.Ack
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.Attack
import it.unibo.tuprolog.argumentation.parallel.message.AttacksFound
import it.unibo.tuprolog.argumentation.parallel.message.Build
import it.unibo.tuprolog.argumentation.parallel.message.DirectAttack
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.Explore
import it.unibo.tuprolog.argumentation.parallel.message.Interest
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.Publish
import it.unibo.tuprolog.argumentation.parallel.message.Rebuild
import it.unibo.tuprolog.argumentation.parallel.message.RequireAnswer
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.RequireSolution
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.message.Response
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse
import it.unibo.tuprolog.argumentation.parallel.message.Subscribe
import it.unibo.tuprolog.argumentation.parallel.partition.Partitioning
import it.unibo.tuprolog.argumentation.parallel.partition.TheoryPartitioner
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.ParseException
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.unify.Unificator
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
 *    on their own (with metaConflicts, where the conflicts are arguments as well, the solver of the catalog finds them);
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
 * graph being built ([Rebuild]), and when a goal needs a graph of the other kind (whole, or in query mode); in query
 * mode, it grows as later goals need more of it.
 */
internal class Catalog(
    private val network: Network,
    private val parts: Int? = null,
    private val flags: Flags = Flags.defaults(),
) : Node {
    private val theory = StringBuilder()
    private var changed = true
    private var workers = 0

    // The theory split for as many workers as the network ran when it was split, and how the solver builds the graph:
    // by the catalog alone (a graph not built from rules), the attacks by the solver (with metaConflicts), and the
    // locality query mode explores (null for the whole graph)
    private var partitioning: Partitioning? = null
    private var partitionedFor: Int? = null
    private var central = false
    private var centralAttacks = false
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

    // The direct attacks the workers found
    private val directAttacks = mutableListOf<DirectAttack>()

    // The graph built, as the modules of the solver save it: the arguments and their supports, the attacks (none if the
    // workers do not find them); and what the goal solved needs that is not built yet
    private var saved: Pair<Term, Term>? = null
    private var savedAttacks: Term? = null
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

    // Whether the solver holds the labels of the graph built (an evaluation of a statement labelled it, and no goal was
    // solved since), whether the job done used the graph built, and the arguments of the graph last reported (see
    // EvaluationStats)
    private var labelled = false
    private var used = false
    private var argumentsReported = 0

    private val solver =
        Arg2pSolverFactory.solverOf(
            Arg2pSolver.of(listOf(Arg2pModule, flags.library()), graphModules(Built())),
            "",
            output = OutputChannel.of { printed.append(it) },
        )
    private val unificator = Unificator.default

    // The arguments for a statement read, by label, and the size of the graph labelled
    private val argumentsLabelled = Goal("graphStore::argumentLabel(L, A), argument::claim(A, S)", solver.operators)
    private val size =
        Goal(
            "graphStore::argumentIds(Ids), length(Ids, Arguments), findall(T, graphStore::attackById(T, _, _, _), Ts), " +
                "length(Ts, Attacks)",
            solver.operators,
        )

    // A message handled: an error (a theory that cannot be read, or that its profile refuses) answers the jobs waiting
    // with it, and the theory is read anew for the next ones
    override fun receive(message: KbMessage) {
        try {
            handle(message)
        } catch (error: Exception) {
            jobs.toList().also { jobs.clear() }.forEach { network.send(it.replyTo, it.failed(error.message ?: error.toString())) }
            stage = Stage.READY
            changed = true
            need = null
        }
    }

    private fun handle(message: KbMessage) {
        when (message) {
            Reset -> change { theory.clear() }
            // a graph lost some of its workers: built again now if being built, at the next goal needing it otherwise
            // (its workers are those of a site that left)
            Rebuild -> if (stage != Stage.READY) start(explored.toList(), whole = locality == null) else built = false
            is Load -> change { theory.clear().append(message.theory).append('\n') }
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
                        runs[message.run]
                    } else {
                        Run { goal(message.goal, it) }.also {
                            runs[message.run] = it
                            if (runs.size > MAX_RUNS) runs.remove(runs.keys.first())
                        }
                    }
                if (run == null) {
                    network.send(message.replyTo, SolutionResponse(message.run, null, NO_GRAPH, false, "", "This goal is no longer solved"))
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
                    arguments[argument.id] = argument.argument
                }
                network.send(Address.worker(message.worker), Ack(-1))
            }
            is Ack -> if (stage == Stage.BUILDING && --awaited == 0) published()
            is AttacksFound -> {
                directAttacks += message.attacks
                if (--awaited == 0) finished()
            }
            else -> Unit
        }
    }

    private fun change(update: () -> Unit) {
        update()
        changed = true
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
            val answer =
                try {
                    job.shortcut() ?: next(job.run).let { solution ->
                        val wanted = need ?: return@let job.answer(solution)
                        need = null
                        return build(wanted)
                    }
                } catch (error: Arg2pException) {
                    job.failed(error.message ?: error.toString())
                }
            jobs.removeFirst()
            // the statistics of a job using the graph, known by its client once answered
            if (used || job is Evaluating) evaluated(job.since)
            used = false
            network.send(job.replyTo, answer)
        }
    }

    // The next solution of [run]: solved from its start if the solver solved another goal since, the solutions given
    // already left out (and what was printed computing them); null if it has no more, or needs a graph not built yet
    private fun next(run: Run): LogicSolution? {
        if (current !== run) {
            current = run
            labelled = false
            printed.clear()
            run.solutions = solver.solve(run.goal(solver)).iterator()
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

    // Whether [run] has more solutions: if knowing needs a graph not built yet, it is built when they are asked for
    private fun hasNext(run: Run): Boolean {
        val more = run.solutions?.hasNext() ?: false
        if (need == null) return more
        need = null
        current = null
        return true
    }

    // The solver given the theory, changed: the part of the catalog (see TheoryPartitioner), and how it builds the graph
    private fun prepare() {
        val partitioning = partitioning()
        solver.resetStaticKb()
        solver.loadStaticKb(Theory.parse(partitioning.catalog, solver.operators))
        solver.solveOnce("context_reset, parser:::convertAllRules(_)")
        central = !solver.holds("structuredWhole::builder(B), B == ruleGraph")
        centralAttacks = central || solver.holds("setting(metaConflicts)")
        partial = if (central) null else locality()
        changed = false
        built = false
        labelled = false
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

    // [need] built by the workers: the whole graph, the statement explored further in query mode on the graph built,
    // or explored on a graph built anew
    private fun build(need: Need) {
        when {
            need is Need.Explored && built && locality != null && locality == partial -> {
                stage = Stage.BUILDING
                built = false
                awaited = 0
                explore(listOf(need.statement))
                if (awaited == 0) published()
            }
            need is Need.Explored -> start(listOf(need.statement), whole = false)
            else -> start(emptyList(), whole = true)
        }
    }

    // Whether the graph built has what [need] asks for
    private fun met(need: Need) =
        built &&
            when (need) {
                Need.Whole -> locality == null
                is Need.Explored -> locality == partial && (locality == null || explored.any { covers(it, need.statement) })
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
        workers = count
        built = false
        interests.clear()
        explored.clear()
        pending.clear()
        if (locality != null) pending += goals
        arguments.clear()
        supports.clear()
        directAttacks.clear()
        argumentsReported = 0
        if (workers == 0) return published()
        stage = Stage.SUBSCRIBING
        awaited = workers
    }

    // The part of the graph the labels of the arguments for a statement depend on, as the semantics declares it (see
    // the module structuredPartial): directional or connected in query mode, on a graph built from the rules; null for
    // the whole graph
    private fun locality(): String? =
        solver
            .solveOnce(
                "setting(queryMode), structuredWhole::builder(B), B == ruleGraph, setting(argumentLabellingMode, S), " +
                    "S::locality(L), L \\== whole_graph",
            ).takeIf { it.isYes }
            ?.substitution
            ?.getByName("L")
            ?.toString()

    // [goals] explored by the workers whose rules conclude them, each Explore acknowledged
    private fun explore(goals: List<Term>) {
        explored += goals
        interests.forEach { interest ->
            val theirs = goals.filter { goal -> interest.conclusions.any { unificator.match(it, goal) } }
            if (theirs.isNotEmpty()) {
                awaited++
                network.send(Address.worker(interest.worker), Explore(-1, theirs))
            }
        }
    }

    // The arguments all published: the workers find the attacks, but with metaConflicts
    private fun published() {
        directAttacks.clear()
        if (workers == 0 || centralAttacks) return finished()
        stage = Stage.ATTACKING
        awaited = workers
        repeat(workers) { network.send(Address.worker(it), Attack) }
    }

    // The graph built, kept as the modules of the solver save it (the attacks as attackBuilder saves them, none if the
    // workers did not find them): the jobs waiting are done on it
    private fun finished() {
        val supported =
            supports.map { (support, argument) ->
                Struct.of("support", arguments.getValue(support), arguments.getValue(argument))
            }
        val attacks =
            directAttacks.distinct().map { (type, attacker, target) ->
                val attacked = arguments.getValue(target)
                Struct.of("attack", Atom.of(type), arguments.getValue(attacker), attacked, attacked)
            }
        saved = PlList.of(arguments.values) to PlList.of(supported)
        savedAttacks = PlList.of(attacks).takeUnless { centralAttacks }
        built = true
        labelled = false
        stage = Stage.READY
        work()
    }

    // The statistics of the job just done, waiting since [since]: what each worker built, read from the global
    // identifiers of its arguments (its partition first) and of the arguments its attacks target
    private fun evaluated(since: TimeMark) {
        val published = arguments.keys.groupingBy { it.substringBefore(':').toInt() }.eachCount()
        val found = directAttacks.distinct().groupingBy { it.target.substringBefore(':').toInt() }.eachCount()
        val built = List(workers) { WorkerStats(it, network.siteOf(Address.worker(it)), published[it] ?: 0, found[it] ?: 0) }
        val (graphArguments, graphAttacks) = size.solutions(solver).first().let { it.count("Arguments") to it.count("Attacks") }
        val added = arguments.size - argumentsReported
        argumentsReported = arguments.size
        network.evaluated(
            EvaluationStats(
                locality ?: "whole_graph",
                graphArguments,
                graphAttacks,
                added,
                built,
                since.elapsedNow().inWholeMicroseconds / 1000.0,
            ),
        )
    }

    private fun Map<String, Term>.count(name: String) = (getValue(name) as Integer).intValue.toInt()

    // The graph of the last solution, none for a solution that labels none
    private fun mined(): Graph =
        try {
            solver.graph()
        } catch (_: Exception) {
            NO_GRAPH
        }

    // The graph built, for the modules of the solver: what a goal needs and the workers did not build is asked for
    private inner class Built : BuiltGraph {
        override fun arguments(need: Term): Pair<Term, Term>? {
            val wanted = if (need is Struct && need.functor == "explore") Need.Explored(need[0]) else Need.Whole
            if (!met(wanted)) {
                this@Catalog.need = wanted
                return null
            }
            used = true
            return saved
        }

        override val attacks: Term? get() = savedAttacks
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

    // A job: the next solution of [run], answered to [replyTo] (shortcut if the solver need not solve it), or the
    // error that kept it from one; it waits [since]
    private abstract inner class Job(
        val replyTo: String,
    ) {
        abstract val run: Run

        val since = TimeSource.Monotonic.markNow()

        open fun shortcut(): KbMessage? = null

        abstract fun answer(solution: LogicSolution?): KbMessage

        abstract fun failed(error: String): KbMessage
    }

    // The next solution of the goal of the client, with the graph of its resolution and what the solver printed
    private inner class Solving(
        private val id: String,
        override val run: Run,
        replyTo: String,
    ) : Job(replyTo) {
        override fun answer(solution: LogicSolution?): KbMessage {
            val graph = solution?.let { mined() } ?: NO_GRAPH
            val more = solution != null && hasNext(run)
            val output = printed.substring(minOf(run.printed, printed.length))
            run.printed += output.length
            return SolutionResponse(id, solution, graph, more, output)
        }

        override fun failed(error: String) = SolutionResponse(id, null, NO_GRAPH, false, "", error)
    }

    // The statements for a query by label, as the standard solver answers them
    private inner class Answering(
        request: RequireAnswer,
    ) : Job(request.replyTo) {
        private val query by lazy { AnswerQuery(term(request.query, solver.operators)) }

        override val run = Run { query.goal }

        override fun answer(solution: LogicSolution?): KbMessage =
            if (solution?.isHalt == true) {
                failed(solution.exception?.message ?: solution.toString())
            } else {
                AnswerResponse(query.answer(solution?.takeIf { it.isYes }))
            }

        override fun failed(error: String) = AnswerResponse(null, error)
    }

    // The arguments for a statement by label: the graph labelled for it (see answerQuery/4), or as it was if the solver
    // labelled the graph built and it has them
    private inner class Evaluating(
        private val request: RequireEvaluation,
        private val statement: Term,
    ) : Job(request.replyTo) {
        override val run = Run { Struct.of("answerQuery", statement, Var.anonymous(), Var.anonymous(), Var.anonymous()) }

        override fun shortcut(): KbMessage? = if (labelled && met(needed)) labels() else null

        override fun answer(solution: LogicSolution?): KbMessage {
            if (solution == null) return EvaluationResponse(emptyList(), emptyList(), emptyList())
            if (solution.isHalt) return failed(solution.exception?.message ?: solution.toString())
            labelled = true
            return labels()
        }

        override fun failed(error: String) = EvaluationResponse(emptyList(), emptyList(), emptyList(), error)

        // what answering the statement needs: the whole graph, unless query mode explores it
        private val needed: Need get() = if (partial == null) Need.Whole else Need.Explored(statement)

        private fun labels(): EvaluationResponse {
            // the statement bound as a term, its variables apart from those of the goal
            val found =
                argumentsLabelled
                    .solutions(solver, "S" to statement)
                    .map { it.getValue("L").toString() to it.getValue("A").toString() }
                    .distinct()
            val labelled = { label: String -> found.filter { it.first == label }.map { Response(request.elem, listOf(it.second)) } }
            val und = if (found.isEmpty()) listOf(Response(request.elem, emptyList())) else labelled("und")
            return EvaluationResponse(labelled("in"), labelled("out"), und)
        }
    }

    private companion object {
        // The goals of the clients kept, to give their next solutions
        const val MAX_RUNS = 16

        val NO_GRAPH = Graph(emptyList(), emptyList(), emptyList())

        // A query, Prolog text, read as the standard solver reads it
        fun term(
            text: String,
            operators: OperatorSet,
        ): Term = SyntaxProblem.parsing("Query") { Term.parse(text.trim().removeSuffix("."), operators) }

        // A goal, Prolog text, read as the standard solver reads it
        fun goal(
            text: String,
            operators: OperatorSet,
        ): Struct = term(text, operators).let { it as? Struct ?: throw Arg2pException("Not a goal: $text") }
    }
}
