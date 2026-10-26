package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.message.Ack
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.Attack
import it.unibo.tuprolog.argumentation.parallel.message.AttacksFound
import it.unibo.tuprolog.argumentation.parallel.message.Build
import it.unibo.tuprolog.argumentation.parallel.message.Candidates
import it.unibo.tuprolog.argumentation.parallel.message.DirectAttack
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.Notify
import it.unibo.tuprolog.argumentation.parallel.message.Publish
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.message.SharedArgument
import it.unibo.tuprolog.argumentation.parallel.message.Subscribe
import it.unibo.tuprolog.argumentation.parallel.message.Wanted
import it.unibo.tuprolog.core.EmptyList
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.unify.Unificator
import it.unibo.tuprolog.core.List as PlList

/**
 * A worker of a distributed solver, at [address]: it owns some of the rules of the theory (see
 * [it.unibo.tuprolog.argumentation.parallel.partition.TheoryPartitioner]) and builds their arguments with the graph
 * builder of arg2p (see the module argumentBuilder), on the arguments of its own rules and on those the other workers
 * send it, kept in its graph as well. It sends each argument of its rules to the workers whose rules use its
 * conclusion, and publishes it to the catalog; then it finds the direct attacks on the arguments of its rules (see
 * [Catalog] and [it.unibo.tuprolog.argumentation.parallel.message.KbMessage]).
 */
class Worker(
    private val address: String,
    private val network: Network,
) : Node {
    private val index = Address.indexOf(address)

    private val solver =
        Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries =
                Arg2pSolver
                    .default(listOf(FlagsBuilder(graphExtensions = emptyList()).create()))
                    .to2pLibraries(),
            flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
        )
    private val unificator = Unificator.default

    // The workers, and the premises of the rules of the others
    private var workers = 0
    private val interests = mutableMapOf<Int, List<Term>>()

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

    private val premises = goal("parser::classicRule(_, Body, _), member(P, Body), P \\= ~(_), P \\= prolog(_)")
    private val kept =
        goal("argument::conclusion(A, [C]), (graphStore::saveArgument(f, [C], A) -> true ; true), graphStore::argumentId(A, Id)")
    private val fire =
        goal(
            "graphStore::forgetNewConclusions(a), graphStore::forgetNewConclusions(b), graphStore::forgetNewConclusions(f), " +
                "argumentBuilder::buildArgumentsFromRules(N, a)",
        )
    private val built =
        goal("graphStore::argumentById(Id, A), Id > L, findall(SId, (graphStore::support(S, A), graphStore::argumentId(S, SId)), Ss)")
    private val conflicts = goal("attackCandidates::findPossibleAttackers(X, Conflict), member(S, Conflict)")
    private val attack = goal("attackRelation::attacks(T, B, X)")

    override fun receive(message: KbMessage) {
        when (message) {
            Reset -> load("")
            is Load -> {
                load(message.theory)
                val needed = solver.solutions(premises.with()).map { it.getValue("P") }.distinctBy { it.toString() }
                network.send(Address.CATALOG, Subscribe(index, needed))
            }
            is Build ->
                engaged(Address.CATALOG) {
                    workers = message.interests.size
                    message.interests.filter { it.worker != index }.forEach { interests[it.worker] = it.premises }
                    solve("argumentBuilder::buildArguments")
                    share()
                }
            is Notify ->
                engaged(Address.worker(message.from)) {
                    val claims = keep(message.arguments)
                    solver.solve(fire.with("N" to PlList.of(claims))).first()
                    share()
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

    private fun load(theory: String) {
        lastId = -1
        received.clear()
        own.clear()
        terms.clear()
        globalIds.clear()
        interests.clear()
        deficit = 0
        parent = null
        attacking = false
        candidatesSent = false
        attacksSent = false
        wantedBy.clear()
        candidates.clear()
        conflictsOf.clear()
        candidatesFrom = 0
        solver.resetStaticKb()
        solver.loadStaticKb(Theory.parse(theory, solver.operators))
        solve("context_reset, parser:::convertAllRules(_)")
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

    // The arguments of the others kept in the graph, their conclusions given
    private fun keep(arguments: List<SharedArgument>): List<Term> =
        arguments.flatMap { argument ->
            val term = argument.argument
            solver.solutions(kept.with("A" to term)).map {
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
        val new = solver.solutions(built.with("L" to Integer.of(lastId))).sortedBy { it.integer("Id") }
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
                id to SharedArgument(globalIds.getValue(id), it.getValue("A"), supports)
            }
        if (mine.isEmpty()) return
        send(Address.CATALOG, Publish(index, mine.map { it.second }))
        interests.forEach { (worker, patterns) ->
            val wanted = mine.filter { (id, _) -> patterns.any { unificator.match(it, claim(terms.getValue(id))) } }
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

    // The statements whose arguments may attack the arguments of its rules, found once for each (see sendAttacks)
    private fun wanted(): List<Term> {
        own.forEach { id -> conflictsOf[id] = solver.solutions(conflicts.with("X" to terms.getValue(id))).map { it.getValue("S") } }
        return conflictsOf.values.flatten().distinctBy { it.toString() }
    }

    // Once the attack search started and every other worker told what it wants, each is sent the arguments of the rules
    // of this worker for it, as much of them as an attack needs: [[], TopRule, Conclusion, Body, Info]
    private fun sendCandidates() {
        if (!attacking || candidatesSent || wantedBy.size < workers - 1) return
        candidatesSent = true
        wantedBy.forEach { (worker, patterns) ->
            val matching = own.filter { id -> patterns.any { unificator.match(it, claim(terms.getValue(id))) } }
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
                        .filter { (_, attacker) -> statements.any { unificator.match(it, claim(attacker)) } }
                        .flatMap { (attackerId, attacker) ->
                            solver
                                .solutions(attack.with("B" to attacker, "X" to target))
                                .map { DirectAttack(it.getValue("T").toString(), attackerId, globalIds.getValue(id)) }
                        }
                }.distinct()
        network.send(Address.CATALOG, AttacksFound(index, attacks))
    }

    // An argument, [Rules, TopRule, [Conclusion], Body, Info]: its conclusion, and itself without its rules
    private fun claim(argument: Term) =
        argument
            .castToList()
            .toList()[2]
            .castToList()
            .toList()
            .first()

    private fun stub(argument: Term) = PlList.of(listOf(EmptyList()) + argument.castToList().toList().drop(1))

    private fun goal(text: String) = Goal(text, solver.operators)

    private fun parse(term: String) = Struct.parse(term, solver.operators)

    private fun solve(goal: String) = solver.solve(Struct.parse(goal, solver.operators)).first()

    private fun Map<String, Term>.integer(name: String) = (getValue(name) as Integer).intValue.toInt()
}
