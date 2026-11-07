package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.checked
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.PortableRecords
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.engine.vm.SolverSession
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.ExportedArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.argumentsSavedAfter
import it.unibo.tuprolog.argumentation.core.primitives.graph.saveArgumentRecords
import it.unibo.tuprolog.argumentation.core.runtime.subsumes
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
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
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
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

    // The workers, the statements each other one needs (the premises of its rules, or those it asked to explore), and
    // the conclusions of the rules of each
    private var workers = 0
    private val interests = mutableMapOf<Int, List<Term>>()
    private val conclusionsOf = mutableMapOf<Int, List<Term>>()

    // Query mode: the locality explored (null on the whole graph), the statements asked to explore, and those this
    // worker asked the others to explore
    private var locality: String? = null
    private val queries = ClaimIndex<Term>(statementOf = { it })
    private val asked = ClaimIndex<Term>(statementOf = { it })
    private val queried = mutableListOf<Term>()
    private val concluding = mutableMapOf<Int, ClaimIndex<Term>>()
    private val handled = mutableSetOf<String>()

    // The arguments in the graph so far (the identifiers grow), those received, those of its rules; the arguments it
    // knows as terms and by their global identifiers, by their identifiers in the graph
    private var lastId = -1
    private val received = mutableSetOf<Int>()
    private val own = mutableListOf<Int>()
    private val ownByClaim = ClaimIndex<Int>(statementOf = ::claim)
    private val terms = mutableMapOf<Int, Term>()
    private val handles = mutableMapOf<Int, SharedArgument>()
    private var exported: ExportedArguments? = null
    private var importing: Pair<PortableRecords, IntArray>? = null
    private var imported = IntArray(0)
    private val claims = mutableMapOf<Int, Term>()
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
    private var candidatesFrom = 0

    private val premises = goal("parser::rule(_, Body, _), member(P, Body), parser::argued(P)")
    private val heads = goal("(parser::rule(_, _, [C]) ; parser::premise(_, [C]))")
    private val explorations = goal("workerGoals::explore")
    private val resumption = goal("workerGoals::resume")
    private val exploredStatements = goal("(queryExploration::explored(S) ; queryExploration::premiseOfExplored(S))")
    private val kept = goal("workerGoals::keptArguments(Kept)")
    private val keptCandidates = goal("workerGoals::keptCandidates(Kept)")
    private val fire = goal("workerGoals::fire")
    private val export = goal("workerGoals::exportSince(L)")
    private val import = goal("workerGoals::importArguments")

    // the arguments built since L, with the identifiers of their supports, and the supports bound as they use them
    // (with constraints, the instances of saved arguments, not saved themselves: see the module constraints)
    private val built = goal("graphStore::savedAfter(L, Id, A, Ss)")
    private val builtBound =
        goal(
            "graphStore::savedAfter(L, Id, A, Ss), " +
                "findall(S, (graphStore::support(S, A), graphStore::argumentId(S, SId), \\+ graphStore::savedId(SId)), Bs)",
        )
    private var constrained = false
    private val conflicts = goal("workerGoals::possibleAttackers(Statements)")
    private val attacksOn = goal("workerGoals::attacksOnOwn(Attacks)")

    // The lists of the goals of the worker, by name, for the module of its goals: a list in the text of a goal is compiled, a
    // register for each cell and its code kept, recursing along it
    private val passed = mutableMapOf<String, Term>()

    private inner class Passed : AbstractNativePrimitive(Signature("passed", 2)) {
        override fun call(call: NativeCall): Int = call.replyWord(1, call.word(passed.getValue(call.atomName(call.arg(0)))))
    }

    // exportSince(+Last): the arguments saved above Last, as records (see argumentsSavedAfter)
    private inner class Export : AbstractNativePrimitive(Signature("exportSince", 1)) {
        override fun call(call: NativeCall): Int {
            exported = call.argumentsSavedAfter(call.intValue(call.arg(0)).toInt())
            return NativeCall.CONTINUE
        }
    }

    // importArguments: the arguments of the batch being imported saved, their conclusions new under f (see saveArgumentRecords)
    private inner class Import : AbstractNativePrimitive(Signature("importArguments", 0)) {
        override fun call(call: NativeCall): Int {
            val (batch, at) = importing!!
            imported = call.saveArgumentRecords("f", batch, at)
            return NativeCall.CONTINUE
        }
    }

    private inner class WorkerGoals : PrologModule("prolog.argumentation.parallel.worker", "workerGoals") {
        override val primitives: Map<Signature, Primitive>
            get() = listOf(Passed(), Export(), Import()).associateBy { it.signature }

        override val prologRawTheory: String =
            // language=Prolog
            """
            explore :-
                passed(queries, Queries),
                queryExploration::exploreAll(Queries).

            resume :-
                passed(claims, Claims),
                passed(queries, Queries),
                queryExploration::resumed(Claims, Queries).

            keptArguments(Kept) :-
                passed(arguments, Arguments),
                findall(k(N, C, Id), (
                    member(n(N, A), Arguments),
                    argument::conclusion(A, [C]),
                    (graphStore::saveArgument(f, [C], A) -> true ; true),
                    graphStore::argumentId(A, Id)
                ), Kept).

            % (saved as instances: two candidates may unify, each an attacker)
            keptCandidates(Kept) :-
                passed(arguments, Arguments),
                findall(k(N, C, Id), (
                    member(n(N, A), Arguments),
                    argument::conclusion(A, [C]),
                    (graphStore::saveInstance([C], A) -> true ; true),
                    graphStore::argumentId(A, Id)
                ), Kept).

            fire :-
                findall(C, graphStore::newConclusion(f, [C]), Claims),
                graphStore::forgetNewConclusions(a),
                graphStore::forgetNewConclusions(b),
                graphStore::forgetNewConclusions(f),
                argumentBuilder::buildArgumentsFromRules(Claims, a).

            possibleAttackers(Statements) :-
                passed(ids, Ids),
                findall(S, (
                    member(Id, Ids),
                    graphStore::argumentById(Id, A),
                    attackCandidates::possibleAttackers(Id, A, Conflict),
                    member(S, Conflict)
                ), Statements).

            attacksOnOwn(Attacks) :-
                passed(ids, Ids),
                attackBuilder::attacksOn(Ids, Attacks).
            """.trimIndent()
    }

    // A message handled: a failure (an error of the theory, as a Prolog check that raises one) told to the catalog,
    // which gives up the graph being built and answers the clients with it, rather than left waiting for a reply (out
    // of memory too: a reply lost to it would leave them waiting for ever)
    override fun receive(message: KbMessage) {
        try {
            handle(message)
        } catch (failure: Throwable) {
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
                            val built = ownByClaim.matchingAny(message.statements)
                            if (built.isNotEmpty()) {
                                send(
                                    Address.worker(message.from),
                                    Notify(index, built.map { shared(it) }),
                                )
                            }
                        }
                        val new = message.statements.filter { statement -> queries.matching(statement).none { it.subsumes(statement) } }
                        new.forEach { queries.add(it) }
                        queried += new
                        passed["queries"] = PlList.of(new)
                        checked(solver.solve(explorations.with()).first())
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
                            checked(solver.solve(fire.with()).first())
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
        solver = Arg2pSolverFactory.solverOf(Arg2pSolver.default(listOf(Flags.parse(flags).library()), listOf(WorkerGoals())), "")
        network.solvers?.let { SolverSession.join(solver, it) }
        solver.loadStaticKb(RuleAnnotations.parse(theory, solver.operators, solver.unificator))
        solver.solveOnce("context_reset, parser:::convertAllRules")
        constrained = solver.holds("parser::constrained")
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
        passed["claims"] = PlList.of(claims)
        passed["queries"] = PlList.of(queried)
        checked(solver.solve(resumption.with()).first())
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
                .filter { statement -> asked.matching(statement).none { it.subsumes(statement) } }
        needed.forEach { asked.add(it) }
        conclusionsOf.forEach { (worker, conclusions) ->
            val concluded = concluding.getOrPut(worker) { ClaimIndex(conclusions, { it }) }
            val theirs = needed.filter { concluded.anyMatching(it) }
            if (theirs.isNotEmpty()) send(Address.worker(worker), Explore(index, theirs))
        }
    }

    // The arguments of the others kept in the graph, their conclusions given
    private fun keep(arguments: List<SharedArgument>): List<Term> {
        val batch = arguments.firstOrNull()?.batch
        if (constrained || batch == null || arguments.any { it.batch !== batch }) {
            return savedArguments(arguments, kept).map { (argument, id, claim) ->
                received += id
                terms[id] = argument.argument
                globalIds[id] = argument.id
                claim
            }
        }
        importing = batch to IntArray(arguments.size) { arguments[it].index }
        checked(solver.solve(import.with()).first())
        importing = null
        return arguments.mapIndexed { i, argument ->
            val id = imported[i]
            received += id
            handles[id] = argument
            globalIds[id] = argument.id
            argument.claim!!.also { claims[id] = it }
        }
    }

    // [arguments] saved in the graph, in one goal: each with its identifier and its conclusion
    private fun savedArguments(
        arguments: List<SharedArgument>,
        saving: Goal,
    ): List<Triple<SharedArgument, Int, Term>> {
        passed["arguments"] = PlList.of(arguments.mapIndexed { i, argument -> Struct.of("n", Integer.of(i), argument.argument) })
        return saving
            .solutions(solver)
            .single()
            .getValue("Kept")
            .castToList()
            .toList()
            .map {
                val saved = it.castToStruct()
                Triple(arguments[(saved[0] as Integer).intValue.toInt()], (saved[2] as Integer).intValue.toInt(), saved[1])
            }
    }

    // The arguments of its rules built since the last time, published with their supports, and sent to the workers
    // whose rules use their conclusions
    private fun share() {
        if (!constrained) return shareRecords()
        val new = (if (constrained) builtBound else built).solutions(solver, "L" to Integer.of(lastId)).sortedBy { it.integer("Id") }
        lastId = maxOf(lastId, new.maxOfOrNull { it.integer("Id") } ?: lastId)
        val mine =
            new.filter { it.integer("Id") !in received }.map {
                val id = it.integer("Id")
                own += id
                terms[id] = it.getValue("A")
                ownByClaim.add(id)
                globalIds[id] = "$index:$id"
                val supports =
                    it.getValue("Ss").castToList().toList().map { support ->
                        globalIds.getValue((support as Integer).intValue.toInt())
                    }
                id to SharedArgument(globalIds.getValue(id), it.getValue("A"), supports, it["Bs"]?.castToList()?.toList().orEmpty())
            }
        if (mine.isEmpty()) return
        send(Address.CATALOG, Publish(index, mine.map { it.second }))
        val mineByClaim = ClaimIndex(mine, { (id, _) -> claim(id) })
        interests.forEach { (worker, patterns) ->
            val wanted = mineByClaim.matchingAny(patterns)
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

    // share() on records: the arguments built since the last time read off the store as they are, no term made of them
    private fun shareRecords() {
        export.solutions(solver, "L" to Integer.of(lastId)).toList()
        val batch = exported!!
        exported = null
        if (batch.ids.isNotEmpty()) lastId = maxOf(lastId, batch.ids.last())
        val mine = ArrayList<Pair<Int, SharedArgument>>()
        for (i in batch.ids.indices) {
            val id = batch.ids[i]
            if (id in received) continue
            own += id
            globalIds[id] = "$index:$id"
            val argument = SharedArgument.of("$index:$id", batch.records, i, batch.supporters[i].map { globalIds.getValue(it) })
            handles[id] = argument
            ownByClaim.add(id)
            mine += id to argument
        }
        if (mine.isEmpty()) return
        send(Address.CATALOG, Publish(index, mine.map { it.second }))
        val mineByClaim = ClaimIndex(mine, { (id, _) -> claim(id) })
        interests.forEach { (worker, patterns) ->
            val wanted = mineByClaim.matchingAny(patterns)
            if (wanted.isNotEmpty()) send(Address.worker(worker), Notify(index, wanted.map { (_, it) -> it.alone() }))
        }
    }

    // The argument of identifier [id] as it is known: from its batch, or as a term
    private fun shared(id: Int): SharedArgument = handles[id]?.alone() ?: SharedArgument(globalIds.getValue(id), terms.getValue(id))

    // The argument of identifier [id] as a term: read off its batch the first time
    private fun term(id: Int): Term = terms[id] ?: handles.getValue(id).argument

    // The statements whose arguments may attack the arguments of its rules, found once
    private fun wanted(): List<Term> =
        conflicts
            .also { passed["ids"] = ownIds() }
            .solutions(solver)
            .single()
            .getValue("Statements")
            .castToList()
            .toList()
            .distinctBy { it.toString() }

    private fun ownIds() = PlList.of(own.map { Integer.of(it) })

    // Once the attack search started and every other worker told what it wants, each is sent the arguments of the rules
    // of this worker for it, as much of them as an attack needs: [[], TopRule, Conclusion, Body, Info]
    private fun sendCandidates() {
        if (!attacking || candidatesSent || wantedBy.size < workers - 1) return
        candidatesSent = true
        wantedBy.forEach { (worker, patterns) ->
            val matching = ownByClaim.matchingAny(patterns)
            network.send(
                Address.worker(worker),
                Candidates(index, matching.map { SharedArgument(globalIds.getValue(it), stub(term(it))) }),
            )
        }
        sendAttacks()
    }

    // Once every other worker sent its candidates, the direct attacks on the arguments of its rules, by those and by the
    // arguments of its own rules, each pair checked once
    private fun sendAttacks() {
        if (!candidatesSent || attacksSent || candidatesFrom < workers - 1) return
        attacksSent = true
        // the candidates in the graph, as attackBuilder finds the attackers there: a candidate may stand for several arguments,
        // differing in what an attack does not read
        val standsFor = mutableMapOf<Int, MutableList<String>>()
        savedArguments(candidates, keptCandidates).forEach { (candidate, id, _) -> standsFor.getOrPut(id) { ArrayList() } += candidate.id }
        passed["ids"] = ownIds()
        val attacks =
            attacksOn
                .solutions(solver)
                .single()
                .getValue("Attacks")
                .castToList()
                .toList()
                .flatMap {
                    val attack = it.castToStruct()
                    val attacker = (attack[1] as Integer).intValue.toInt()
                    val target = globalIds.getValue((attack[2] as Integer).intValue.toInt())
                    val type = attack[0].toString()
                    (standsFor[attacker] ?: listOfNotNull(globalIds[attacker])).map { from -> DirectAttack(type, from, target) }
                }.distinct()
        network.send(Address.CATALOG, AttacksFound(index, attacks))
        release()
    }

    // Nothing more is asked of this worker (a graph built again has workers of its own): what it keeps freed, for the labelling
    // of the catalog and the workers still at work
    private fun release() {
        terms.clear()
        handles.clear()
        claims.clear()
        globalIds.clear()
        candidates.clear()
        wantedBy.clear()
        own.clear()
        ownByClaim.clear()
        received.clear()
        passed.clear()
        solver.solveOnce("context_reset")
    }

    // The statement the argument of identifier [id] concludes, read once
    private fun claim(id: Int): Term = claims.getOrPut(id) { handles[id]?.claim ?: claimOf(term(id)) }

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
