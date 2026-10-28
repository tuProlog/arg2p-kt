package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.mining.activeBranch
import it.unibo.tuprolog.argumentation.core.model.mining.arguments
import it.unibo.tuprolog.argumentation.parallel.message.Ack
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
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
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.message.Response
import it.unibo.tuprolog.argumentation.parallel.message.Subscribe
import it.unibo.tuprolog.argumentation.parallel.partition.TheoryPartitioner
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.ParseException
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.unify.Unificator
import it.unibo.tuprolog.core.List as PlList

/**
 * The catalog of a distributed solver: it holds the theory and partitions it among the workers (see
 * [TheoryPartitioner], at most [parts] partitions if given, at most as many as the network can run otherwise), each
 * owning some of the rules, and gathers the graph they build (see [it.unibo.tuprolog.argumentation.parallel.message.KbMessage]
 * for the protocol):
 * 1. each worker is sent its rules and the flags, and tells the premises and the conclusions of its rules; every
 *    worker is then told those of all;
 * 2. the workers build the arguments, sending each one to the workers that build on it and publishing it here: those
 *    of all their rules, or, in query mode (as the standard solver, see the module queryExploration), those the labels
 *    of the statements evaluated depend on, as the semantics declares (directional or connected), explored from the
 *    workers that conclude them;
 * 3. once every Build and Explore is acknowledged, the arguments are all published: the workers find the direct attacks
 *    on their own (with metaConflicts, where the conflicts are arguments as well, the catalog builds the attacks
 *    itself), which the catalog saves as the graph builder does (the graph store extends them to the arguments built on
 *    the attacked ones);
 * 4. the catalog labels the graph as the standard solver does with [flags] and the flags of the theory (every
 *    semantics, every graph extension).
 *
 * The catalog knows the shared part of the theory (the flags, the preferences, the conflicts), not the rules, and
 * relays no argument. An evaluation reads the labels of the arguments for the statement. The graph is built again
 * when the theory changes, and when the network loses some of the workers of a graph being built ([Rebuild]); in query
 * mode, it grows as later evaluations need more of it.
 */
internal class Catalog(
    private val network: Network,
    private val parts: Int? = null,
    private val flags: Flags = Flags.defaults(),
) : Node {
    private val theory = StringBuilder()
    private var changed = true
    private var workers = 0

    // Query mode: the locality explored (null on the whole graph), and the statements explored so far, or to explore
    // once the workers are ready
    private var locality: String? = null
    private var central = false
    private val explored = mutableListOf<Term>()
    private val pending = mutableListOf<Term>()

    // The construction of the graph: the stage it is at, the replies it still waits for (the subscriptions, the
    // acknowledgements of the Builds and Explores, the attacks of each worker), the premises and conclusions of each
    // worker, the evaluations waiting
    private enum class Stage { READY, SUBSCRIBING, BUILDING, ATTACKING }

    private var stage = Stage.READY
    private var awaited = 0
    private val interests = mutableListOf<Interest>()
    private val waiting = mutableListOf<Pair<RequireEvaluation, Term>>()

    // The arguments published, by their global identifiers, and their supports
    private val arguments = mutableMapOf<String, Term>()

    // The arguments of the graph built, those the workers published, for the tests
    internal val argumentsBuilt: Int get() = arguments.size

    // The arguments of the graph labelled, those derived from the others included, as terms, for the tests
    internal val graphArguments: List<Term> get() = solver.arguments(solver.activeBranch()).map { it.termRepresentation() }
    private val supports = mutableListOf<Pair<String, String>>()

    // The direct attacks the workers found, extended once they all have
    private val directAttacks = mutableListOf<DirectAttack>()

    private val solver = Arg2pSolverFactory.default(settings = flags.library())
    private val unificator = Unificator.default

    // The graph saved a batch at a time, and the arguments for a statement read, by label
    private val save = Goal("graphStore::saveArguments(a, Arguments), graphStore::saveSupports(Supports)", solver.operators)
    private val saveAttacks = Goal("graphStore::saveAttacks(Attacks)", solver.operators)
    private val argumentsLabelled = Goal("graphStore::argumentLabel(L, A), argument::claim(A, S)", solver.operators)

    // A message handled: an error (a theory that cannot be read, or that its profile refuses) answers the evaluations
    // waiting with it, and the graph is built anew for the next ones
    override fun receive(message: KbMessage) {
        try {
            handle(message)
        } catch (error: Exception) {
            val answer = EvaluationResponse(emptyList(), emptyList(), emptyList(), error.message ?: error.toString())
            waiting.toList().also { waiting.clear() }.forEach { (request, _) -> network.send(request.replyTo, answer) }
            stage = Stage.READY
            changed = true
        }
    }

    private fun handle(message: KbMessage) {
        when (message) {
            Reset -> change { theory.clear() }
            // a graph lost some of its workers: built again now if being built, at the next evaluation otherwise (its
            // workers are those of a site that left)
            Rebuild -> if (stage != Stage.READY) start(explored.toList()) else changed = true
            is Load -> change { theory.clear().append(message.theory).append('\n') }
            is Add -> change { theory.append(message.elem.trimEnd().removeSuffix(".")).append(".\n") }
            is RequireEvaluation ->
                try {
                    waiting += message to Struct.parse(message.elem, solver.operators)
                    if (stage == Stage.READY) evaluate()
                } catch (error: ParseException) {
                    val response =
                        EvaluationResponse(emptyList(), emptyList(), emptyList(), "Not a statement: ${message.elem} (${error.message})")
                    network.send(message.replyTo, response)
                }
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
                    argument.supports.forEach { supports += it to argument.id }
                    arguments[argument.id] = argument.argument
                }
                network.send(Address.worker(message.worker), Ack(-1))
            }
            is Ack -> if (stage == Stage.BUILDING && --awaited == 0) built()
            is AttacksFound -> {
                directAttacks += message.attacks
                if (--awaited == 0) {
                    saveAttacks()
                    complete(centralAttacks = false)
                }
            }
            else -> Unit
        }
    }

    private fun change(update: () -> Unit) {
        update()
        changed = true
    }

    // The evaluations waiting answered, on a graph built anew if the theory changed, explored further in query mode if
    // they need statements not explored yet
    private fun evaluate() {
        if (waiting.isEmpty()) return
        val goals = waiting.map { it.second }
        when {
            changed -> start(goals)
            locality == null -> serve()
            else -> {
                val unexplored = goals.filter { goal -> explored.none { covers(it, goal) } }
                if (unexplored.isEmpty()) return serve()
                stage = Stage.BUILDING
                awaited = 0
                explore(unexplored)
                if (awaited == 0) built()
            }
        }
    }

    // The theory partitioned again among the workers, the workers loaded with their rules and the flags, and the
    // locality of the graph chosen: [goals] are explored once the workers are ready. A graph built otherwise than from
    // rules (an abstract framework, see graphBuilder) is built by the catalog alone.
    private fun start(goals: List<Term>) {
        val partitioning = TheoryPartitioner.partition(theory.toString(), parts ?: network.capacity, flags)
        solver.resetStaticKb()
        solver.loadStaticKb(Theory.parse(partitioning.catalog, solver.operators))
        solver.solveOnce("context_reset, parser:::convertAllRules(_)")
        central = !solver.holds("structuredWhole::builder(B), B == ruleGraph")
        locality = if (central) null else locality()
        val count = if (central) 0 else partitioning.partitions.size
        network.building(count)
        repeat(count) { network.send(Address.worker(it), Load(partitioning.theoryOf(it), flags.toString())) }
        workers = count
        changed = false
        interests.clear()
        explored.clear()
        pending.clear()
        if (locality != null) pending += goals
        arguments.clear()
        supports.clear()
        directAttacks.clear()
        if (workers == 0) return built()
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

    // The arguments all published: the graph saved anew, the arguments and their supports, then the attacks found by
    // the workers, but with metaConflicts
    private fun built() {
        solver.solveOnce("context_reset, parser:::convertAllRules(_)")
        val supported =
            supports.map { (support, argument) ->
                Struct.of("support", arguments.getValue(support), arguments.getValue(argument))
            }
        checked(solver.solve(save.with("Arguments" to PlList.of(arguments.values), "Supports" to PlList.of(supported))).first())
        directAttacks.clear()
        if (workers == 0 || solver.holds("setting(metaConflicts)")) {
            return complete(centralAttacks = true)
        }
        stage = Stage.ATTACKING
        awaited = workers
        repeat(workers) { network.send(Address.worker(it), Attack) }
    }

    // The direct attacks saved, as attackBuilder saves them: the graph store extends them to the arguments built on
    // the attacked ones
    private fun saveAttacks() {
        val attacks =
            directAttacks.distinct().map { (type, attacker, target) ->
                val attacked = arguments.getValue(target)
                Struct.of("attack", Atom.of(type), arguments.getValue(attacker), attacked, attacked)
            }
        checked(solver.solve(saveAttacks.with("Attacks" to PlList.of(attacks))).first())
    }

    // The graph whole: the arguments derived from the others, the attacks (if the workers did not find them), the
    // extensions and the labels; a graph not built from rules, all of it, as the standard solver does. An error (a
    // graph extension refusing the theory) is raised.
    private fun complete(centralAttacks: Boolean) {
        val attacks = if (centralAttacks) ", X:::buildAttacks" else ""
        checked(
            solver.solveOnce(
                if (central) {
                    "structuredWhole:::computeGlobalAcceptance"
                } else {
                    "structuredWhole::builder(X), X:::buildAdditionalArguments$attacks, structuredWhole:::modifyGraph, " +
                        "structuredWhole:::buildArgumentLabelling, structuredWhole:::buildStatementLabelling"
                },
            ),
        )
        stage = Stage.READY
        evaluate()
    }

    // The evaluations waiting, answered from the labels of the graph
    private fun serve() {
        val requests = waiting.toList()
        waiting.clear()
        requests.forEach { (request, statement) ->
            // the statement bound as a term, its variables apart from those of the goal
            val found =
                argumentsLabelled
                    .solutions(solver, "S" to statement)
                    .map { it.getValue("L").toString() to it.getValue("A").toString() }
                    .distinct()
            val labelled = { label: String -> found.filter { it.first == label }.map { Response(request.elem, listOf(it.second)) } }
            val und = if (found.isEmpty()) listOf(Response(request.elem, emptyList())) else labelled("und")
            network.send(request.replyTo, EvaluationResponse(labelled("in"), labelled("out"), und))
        }
    }
}
