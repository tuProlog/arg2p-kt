package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.context.GraphStoreBatch
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.message.Ack
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.Attack
import it.unibo.tuprolog.argumentation.parallel.message.AttacksFound
import it.unibo.tuprolog.argumentation.parallel.message.Build
import it.unibo.tuprolog.argumentation.parallel.message.DirectAttack
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
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
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

/**
 * The catalog of a distributed solver: it holds the theory and partitions it among the workers (see
 * [TheoryPartitioner], at most [parts] partitions if given, at most as many as the network can run otherwise), each
 * owning some of the rules, and gathers the graph they build (see [it.unibo.tuprolog.argumentation.parallel.message.KbMessage] for the protocol):
 * 1. each worker is sent its rules, and tells the premises they use; every worker is then told those of all, and
 *    builds the arguments of its rules, sending each one to the workers that build on it and publishing it here;
 * 2. once every Build is acknowledged, the arguments are all published: the workers find the direct attacks on their
 *    own (with metaConflicts, where the conflicts are arguments as well, the catalog builds the attacks itself), which
 *    the catalog saves as the graph builder does (the graph store extends them to the arguments built on the attacked
 *    ones);
 * 3. the catalog labels the graph as the standard solver does with [settings] and the flags of the theory (every
 *    semantics, every graph extension).
 *
 * The catalog knows the shared part of the theory (the flags, the preferences, the conflicts), not the rules, and
 * relays no argument. An evaluation reads the labels of the arguments for the statement. The graph is built again
 * when the theory changes, and when the network loses some of the workers of a graph being built ([Rebuild]).
 */
class Catalog(
    private val network: Network,
    private val parts: Int? = null,
    settings: FlagsBuilder = FlagsBuilder(),
) : Node {
    private val theory = StringBuilder()
    private var changed = false
    private var workers = 0

    // The construction of the graph: the stage it is at, the replies it still waits for (the subscriptions, the
    // acknowledgements of the Builds, the attacks of each worker), the premises of each worker, the evaluations waiting
    private enum class Stage { READY, SUBSCRIBING, BUILDING, ATTACKING }

    private var stage = Stage.READY
    private var awaited = 0
    private val interests = mutableListOf<Interest>()
    private val waiting = mutableListOf<RequireEvaluation>()

    // The arguments published, by their global identifiers, and their supports (saved once all are published)
    private val arguments = mutableMapOf<String, Term>()
    private val supports = mutableListOf<Pair<String, String>>()

    // The direct attacks the workers found, extended once they all have
    private val directAttacks = mutableListOf<DirectAttack>()

    private val solver = Arg2pSolverFactory.default(settings = settings.create())

    // The graph is saved in its context a batch at a time (see GraphStoreBatch)
    private val context = Context.of(solver.libraries)!!

    override fun receive(message: KbMessage) {
        when (message) {
            Reset -> change { theory.clear() }
            Rebuild -> if (stage != Stage.READY) start()
            is Load -> change { theory.clear().append(message.theory).append('\n') }
            is Add -> change { theory.append(message.elem.trimEnd().removeSuffix(".")).append(".\n") }
            is RequireEvaluation -> {
                waiting += message
                if (stage == Stage.READY) {
                    if (changed) start() else serve()
                }
            }
            is Subscribe -> {
                interests += Interest(message.worker, message.premises)
                if (--awaited == 0) {
                    stage = Stage.BUILDING
                    awaited = workers
                    repeat(workers) { network.send(Address.worker(it), Build(interests.toList())) }
                }
            }
            is Publish -> {
                val terms =
                    message.arguments.map { argument ->
                        argument.supports.forEach { supports += it to argument.id }
                        argument.argument.also { arguments[argument.id] = it }
                    }
                GraphStoreBatch.saveArguments(context, Atom.of("a"), terms)
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

    // The theory partitioned again: the catalog keeps the shared part, each worker is given its rules (those left over
    // forget theirs)
    private fun start() {
        val partitioning = TheoryPartitioner.partition(theory.toString(), parts ?: network.capacity)
        solver.resetStaticKb()
        solver.loadStaticKb(Theory.parse(partitioning.shared, solver.operators))
        solve("context_reset, parser:::convertAllRules(_)")
        val count = partitioning.partitions.size
        network.building(count)
        repeat(count) { network.send(Address.worker(it), Load(partitioning.theoryOf(it))) }
        (count until workers).forEach { network.send(Address.worker(it), Reset) }
        workers = count
        changed = false
        interests.clear()
        arguments.clear()
        supports.clear()
        directAttacks.clear()
        if (workers == 0) return complete(centralAttacks = true)
        stage = Stage.SUBSCRIBING
        awaited = workers
    }

    // The arguments all published: their supports saved, then the attacks found by the workers, but with metaConflicts
    private fun built() {
        GraphStoreBatch.saveSupports(
            context,
            supports.map { (support, argument) ->
                arguments.getValue(support) to
                    arguments.getValue(argument)
            },
        )
        if (solver.solve(Struct.parse("setting(metaConflicts)", solver.operators)).first().isYes) {
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
                GraphStoreBatch.Attack(Atom.of(type), arguments.getValue(attacker), attacked, attacked)
            }
        GraphStoreBatch.saveAttacks(context, attacks)
    }

    // The graph whole: the arguments derived from the others, the attacks (if the workers did not find them), the
    // extensions and the labels
    private fun complete(centralAttacks: Boolean) {
        val attacks = if (centralAttacks) ", X:::buildAttacks" else ""
        solve(
            "setting(graphBuildMode, X), X:::buildAdditionalArguments$attacks, structuredWhole:::modifyGraph, " +
                "structuredWhole:::buildArgumentLabelling, structuredWhole:::buildStatementLabelling",
        )
        stage = Stage.READY
        serve()
    }

    // The evaluations waiting, answered from the labels of the graph
    private fun serve() {
        val requests = waiting.toList()
        waiting.clear()
        requests.forEach { request ->
            val found =
                solver
                    .solve(Struct.parse("graphStore::argumentLabel(L, A), argument::claim(A, ${request.elem})", solver.operators))
                    .filter { it.isYes }
                    .map { it.substitution.getByName("L").toString() to it.substitution.getByName("A").toString() }
                    .toList()
                    .distinct()
            val labelled = { label: String -> found.filter { it.first == label }.map { Response(request.elem, listOf(it.second)) } }
            val und = if (found.isEmpty()) listOf(Response(request.elem, emptyList())) else labelled("und")
            network.send(request.replyTo, EvaluationResponse(labelled("in"), labelled("out"), und))
        }
    }

    private fun parse(term: String) = Struct.parse(term, solver.operators)

    private fun solve(goal: String) = solver.solve(Struct.parse(goal, solver.operators)).first()
}
