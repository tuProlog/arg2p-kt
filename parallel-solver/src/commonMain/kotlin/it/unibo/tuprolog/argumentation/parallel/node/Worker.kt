package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.checked
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.core.runtime.subsumes
import it.unibo.tuprolog.argumentation.parallel.message.Ack
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.Attack
import it.unibo.tuprolog.argumentation.parallel.message.AttacksFound
import it.unibo.tuprolog.argumentation.parallel.message.Build
import it.unibo.tuprolog.argumentation.parallel.message.Candidates
import it.unibo.tuprolog.argumentation.parallel.message.DirectAttack
import it.unibo.tuprolog.argumentation.parallel.message.Explore
import it.unibo.tuprolog.argumentation.parallel.message.Failed
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.Notify
import it.unibo.tuprolog.argumentation.parallel.message.Publish
import it.unibo.tuprolog.argumentation.parallel.message.SharedArgument
import it.unibo.tuprolog.argumentation.parallel.message.Subscribe
import it.unibo.tuprolog.argumentation.parallel.message.Wanted
import it.unibo.tuprolog.core.EmptyList
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.unify.Unificator
import it.unibo.tuprolog.core.List as PlList

/**
 * A worker of a distributed solver, at [address]: it owns some of the rules of the theory (see
 * [it.unibo.tuprolog.argumentation.parallel.partition.TheoryPartitioner]) and builds their arguments, on the arguments
 * of its own rules and on those the other workers send it, kept in its graph as well: all of them with the graph
 * builder of arg2p (see the module argumentBuilder), or, in query mode, those the evaluations depend on, explored as the
 * standard solver explores a query (see the module queryExploration). It sends each argument of its rules to the
 * workers that need its conclusion, and publishes it to the catalog; then it finds the direct attacks on the arguments
 * of its rules (see [Catalog] and [it.unibo.tuprolog.argumentation.parallel.message.KbMessage]).
 */
internal class Worker(
    private val address: String,
    private val network: Network,
) : Node {
    private val index = Address.indexOf(address)

    // The solver of its rules, with the flags of the catalog: made as the worker is loaded, once (a graph built again has
    // workers of its own)
    private lateinit var solver: MutableSolver
    private val unificator = Unificator.default

    // The workers, the statements each other one needs (the premises of its rules, or those it asked to explore), and
    // the conclusions of the rules of each
    private var workers = 0
    private val interests = mutableMapOf<Int, List<Term>>()
    private val conclusionsOf = mutableMapOf<Int, List<Term>>()

    // Query mode: the locality explored (null on the whole graph), the statements asked to explore, and those this
    // worker asked the others to explore
    private var locality: String? = null
    private val queries = mutableListOf<Term>()
    private val asked = mutableListOf<Term>()
    private val handled = mutableSetOf<String>()

    // The arguments in the graph so far (the identifiers grow), those received, those of its rules; the arguments it
    // knows as terms and by their global identifiers, by their identifiers in the graph
    private var lastId = -1
    private val received = mutableSetOf<Int>()
    private val own = mutableListOf<Int>()
    private val terms = mutableMapOf<Int, Term>()
    private val globalIds = mutableMapOf<Int, String>()

    // The termination of the construction (Dijkstra and Scholten): the messages sent and not yet acknowledged, and the
    // node whose message engaged this worker, acknowledged once all the worker sent is
    private var deficit = 0
    private var parent: String? = null

    // The messages other workers sent before the Build of the catalog reached this one, by another way: handled once
    // it has, as the worker only then knows the workers to send what it builds on them
    private var building = false
    private val early = mutableListOf<KbMessage>()

    // The search for the attacks: started, the statements each other worker wants, the arguments of the others sent to
    // it, how many workers sent theirs
    private var attacking = false
    private var candidatesSent = false
    private var attacksSent = false
    private val wantedBy = mutableMapOf<Int, List<Term>>()
    private val candidates = mutableListOf<SharedArgument>()

    // The statements whose arguments may attack each argument of its rules
    private val conflictsOf = mutableMapOf<Int, List<Term>>()
    private var candidatesFrom = 0

    private val premises = goal("parser::rule(_, Body, _), member(P, Body), parser::argued(P)")
    private val heads = goal("(parser::rule(_, _, [C]) ; parser::premise(_, [C]))")
    private val explorations = goal("queryExploration::exploreAll(Q)")
    private val resumption = goal("queryExploration::resumed(S, Q)")
    private val exploredStatements = goal("(queryExploration::explored(S) ; queryExploration::premiseOfExplored(S))")
    private val kept =
        goal("argument::conclusion(A, [C]), (graphStore::saveArgument(f, [C], A) -> true ; true), graphStore::argumentId(A, Id)")
    private val fire =
        goal(
            "graphStore::forgetNewConclusions(a), graphStore::forgetNewConclusions(b), graphStore::forgetNewConclusions(f), " +
                "argumentBuilder::buildArgumentsFromRules(N, a)",
        )

    // the arguments built since L, with the identifiers of their supports, and the supports bound as they use them
    // (with constraints, the instances of saved arguments, not saved themselves: see the module constraints)
    private val built =
        goal(
            "graphStore::argumentById(Id, A), Id > L, " +
                "findall(SId, (graphStore::support(S, A), graphStore::argumentId(S, SId), graphStore::argumentById(SId, _)), Ss), " +
                "findall(S, (graphStore::support(S, A), graphStore::argumentId(S, SId), \\+ graphStore::argumentById(SId, _)), Bs)",
        )
    private val conflicts = goal("attackCandidates::findPossibleAttackers(X, Conflict), member(S, Conflict)")
    private val attack = goal("attackBuilder::directAttack(T, B, X)")

    // A message handled: a failure (an error of the theory, as a Prolog check that raises one) told to the catalog,
    // which gives up the graph being built and answers the clients with it, rather than left waiting for a reply
    override fun receive(message: KbMessage) {
        try {
            handle(message)
        } catch (failure: Exception) {
            network.send(Address.CATALOG, Failed(index, failure.message ?: failure.toString()))
        }
    }

    private fun handle(message: KbMessage) {
        when (message) {
            is Load -> {
                load(message.theory, message.flags)
                val needed = premises.solutions(solver).map { it.getValue("P") }.distinctBy { it.toString() }
                val concluded = heads.solutions(solver).map { it.getValue("C") }.distinctBy { it.toString() }
                network.send(Address.CATALOG, Subscribe(index, needed, concluded))
            }
            is Build -> {
                engaged(Address.CATALOG) {
                    workers = message.interests.size
                    locality = message.locality
                    message.interests.filter { it.worker != index }.forEach {
                        conclusionsOf[it.worker] = it.conclusions
                        // on the whole graph, the others need the arguments for their premises; in query mode, those
                        // they ask for
                        if (locality == null) interests[it.worker] = it.premises
                    }
                    if (locality == null) {
                        checked(solver.solveOnce("argumentBuilder::buildArguments"))
                        share()
                    }
                }
                building = true
                early.toList().also { early.clear() }.forEach { handle(it) }
            }
            is Explore ->
                if (!building) {
                    early += message
                } else {
                    engaged(if (message.from < 0) Address.CATALOG else Address.worker(message.from)) {
                        if (message.from >= 0) {
                            interests[message.from] = interests[message.from].orEmpty() + message.statements
                            // the arguments for them built before: those built from now on go with the others
                            val built = own.filter { id -> message.statements.any { unificator.match(it, claimOf(terms.getValue(id))) } }
                            if (built.isNotEmpty()) {
                                send(
                                    Address.worker(message.from),
                                    Notify(index, built.map { SharedArgument(globalIds.getValue(it), terms.getValue(it)) }),
                                )
                            }
                        }
                        val new = message.statements.filter { statement -> queries.none { it.subsumes(statement) } }
                        queries += new
                        checked(solver.solve(explorations.with("Q" to PlList.of(new))).first())
                        explored()
                    }
                }
            is Notify ->
                if (!building) {
                    early += message
                } else {
                    engaged(Address.worker(message.from)) {
                        val claims = keep(message.arguments)
                        if (locality == null) {
                            checked(solver.solve(fire.with("N" to PlList.of(claims))).first())
                            share()
                        } else {
                            resume(claims)
                        }
                    }
                }
            is Ack -> {
                deficit--
                detach()
            }
            Attack -> {
                attacking = true
                val statements = wanted()
                (0 until workers).filter { it != index }.forEach { network.send(Address.worker(it), Wanted(index, statements)) }
                sendCandidates()
            }
            is Wanted -> {
                wantedBy[message.worker] = message.statements
                sendCandidates()
            }
            is Candidates -> {
                candidates += message.arguments
                candidatesFrom++
                sendAttacks()
            }
            else -> Unit
        }
    }

    // The solver of the rules of the worker, [theory], with [flags], as Prolog facts (the catalog has the same)
    private fun load(
        theory: String,
        flags: String,
    ) {
        solver = Arg2pSolverFactory.solverOf(Arg2pSolver.default(listOf(Flags.parse(flags).library())), "")
        solver.loadStaticKb(Theory.parse(theory, solver.operators))
        solver.solveOnce("context_reset, parser:::convertAllRules")
    }

    // A message from sender handled: the first one engages the worker (sender acknowledged once all the worker sent
    // is), the others are acknowledged as soon as they are handled
    private fun engaged(
        sender: String,
        handle: () -> Unit,
    ) {
        val engaging = parent == null
        if (engaging) parent = sender
        handle()
        if (!engaging) network.send(sender, Ack(index))
        detach()
    }

    private fun detach() {
        val engagedBy = parent
        if (deficit == 0 && engagedBy != null) {
            parent = null
            network.send(engagedBy, Ack(index))
        }
    }

    private fun send(
        to: String,
        message: KbMessage,
    ) {
        deficit++
        network.send(to, message)
    }

    // Query mode: the exploration of the statements asked carried on with the arguments for [claims] the others sent
    // (see queryExploration::resumed/2)
    private fun resume(claims: List<Term>) {
        checked(solver.solve(resumption.with("S" to PlList.of(claims), "Q" to PlList.of(queries))).first())
        explored()
    }

    // Query mode, once the statements asked are explored: those the arguments built depend on as the locality goes, the
    // arguments of its rules built shared, and the others asked to explore the statements explored that their rules
    // conclude
    private fun explored() {
        checked(solver.solveOnce("queryExploration::expand($locality), queryExploration::extend($locality)"))
        share()
        // the statements explored since the last time (by their text), with the premises of the rules for them (asked
        // all at once, so that the others build their arguments side by side: the exploration would ask for each once
        // the one before it is supported), and of those the ones not asked yet
        val needed =
            exploredStatements
                .solutions(solver)
                .map { it.getValue("S") }
                .filter { handled.add(it.toString()) }
                .filter { statement -> asked.none { it.subsumes(statement) } }
        asked += needed
        conclusionsOf.forEach { (worker, conclusions) ->
            val theirs = needed.filter { statement -> conclusions.any { unificator.match(it, statement) } }
            if (theirs.isNotEmpty()) send(Address.worker(worker), Explore(index, theirs))
        }
    }

    // The arguments of the others kept in the graph, their conclusions given
    private fun keep(arguments: List<SharedArgument>): List<Term> =
        arguments.flatMap { argument ->
            val term = argument.argument
            kept.solutions(solver, "A" to term).map {
                val id = it.integer("Id")
                received += id
                terms[id] = term
                globalIds[id] = argument.id
                it.getValue("C")
            }
        }

    // The arguments of its rules built since the last time, published with their supports, and sent to the workers
    // whose rules use their conclusions
    private fun share() {
        val new = built.solutions(solver, "L" to Integer.of(lastId)).sortedBy { it.integer("Id") }
        lastId = maxOf(lastId, new.maxOfOrNull { it.integer("Id") } ?: lastId)
        val mine =
            new.filter { it.integer("Id") !in received }.map {
                val id = it.integer("Id")
                own += id
                terms[id] = it.getValue("A")
                globalIds[id] = "$index:$id"
                val supports =
                    it.getValue("Ss").castToList().toList().map { support ->
                        globalIds.getValue((support as Integer).intValue.toInt())
                    }
                id to SharedArgument(globalIds.getValue(id), it.getValue("A"), supports, it.getValue("Bs").castToList().toList())
            }
        if (mine.isEmpty()) return
        send(Address.CATALOG, Publish(index, mine.map { it.second }))
        interests.forEach { (worker, patterns) ->
            val wanted = mine.filter { (id, _) -> patterns.any { unificator.match(it, claimOf(terms.getValue(id))) } }
            if (wanted.isNotEmpty()) {
                send(
                    Address.worker(worker),
                    Notify(
                        index,
                        wanted.map { (_, it) ->
                            SharedArgument(it.id, it.argument)
                        },
                    ),
                )
            }
        }
    }

    // The statements whose arguments may attack the arguments of its rules, found once
    private fun wanted(): List<Term> {
        own.filter { it !in conflictsOf }.forEach { id ->
            conflictsOf[id] = conflicts.solutions(solver, "X" to terms.getValue(id)).map { it.getValue("S") }
        }
        return conflictsOf.values.flatten().distinctBy { it.toString() }
    }

    // Once the attack search started and every other worker told what it wants, each is sent the arguments of the rules
    // of this worker for it, as much of them as an attack needs: [[], TopRule, Conclusion, Body, Info]
    private fun sendCandidates() {
        if (!attacking || candidatesSent || wantedBy.size < workers - 1) return
        candidatesSent = true
        wantedBy.forEach { (worker, patterns) ->
            val matching = own.filter { id -> patterns.any { unificator.match(it, claimOf(terms.getValue(id))) } }
            network.send(
                Address.worker(worker),
                Candidates(index, matching.map { SharedArgument(globalIds.getValue(it), stub(terms.getValue(it))) }),
            )
        }
        sendAttacks()
    }

    // Once every other worker sent its candidates, the direct attacks on the arguments of its rules, by those and by the
    // arguments of its own rules, each pair checked once
    private fun sendAttacks() {
        if (!candidatesSent || attacksSent || candidatesFrom < workers - 1) return
        attacksSent = true
        val attackers =
            own.map { globalIds.getValue(it) to terms.getValue(it) } + candidates.map { it.id to it.argument }
        val attacks =
            own
                .flatMap { id ->
                    val target = terms.getValue(id)
                    val statements = conflictsOf.getValue(id)
                    attackers
                        .filter { (_, attacker) -> statements.any { unificator.match(it, claimOf(attacker)) } }
                        .flatMap { (attackerId, attacker) ->
                            attack
                                .solutions(solver, "B" to attacker, "X" to target)
                                .map { DirectAttack(it.getValue("T").toString(), attackerId, globalIds.getValue(id)) }
                        }
                }.distinct()
        network.send(Address.CATALOG, AttacksFound(index, attacks))
    }

    // An argument as much of it as an attack needs: itself without its rules
    private fun stub(argument: Term) =
        ArgumentTerm.parts(argument)!!.let {
            ArgumentTerm.of(
                EmptyList(),
                it.topRule,
                it.conclusion,
                it.body,
                ArgumentTerm.info(it.lastDefeasibleRules, it.defeasibleRules, it.defeasiblePremises),
            )
        }

    private fun goal(text: String) = Goal(text, TermValues.operators)
}
