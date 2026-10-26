package it.unibo.tuprolog.argumentation.parallel.message

import it.unibo.tuprolog.core.Term

/** The label of an argument, NOT_FOUND for a statement no argument concludes */
enum class Label {
    IN,
    OUT,
    UND,
    NOT_FOUND,
}

/** An argument for the statement evaluated ([claim], the statement), with the argument itself in [queryChain] */
data class Response(
    val claim: String,
    val queryChain: List<String>,
)

/**
 * An argument, by its global identifier ([id], the worker that built it and its identifier there), with the identifiers
 * of the arguments it rests on directly ([supports], from a worker to the catalog only)
 */
data class SharedArgument(
    val id: String,
    val argument: Term,
    val supports: List<String> = emptyList(),
)

/** The premises of the rules of [worker] */
data class Interest(
    val worker: Int,
    val premises: List<Term>,
)

/** A direct attack of type [type] of the argument [attacker] on the argument [target], by their global identifiers */
data class DirectAttack(
    val type: String,
    val attacker: String,
    val target: String,
)

/**
 * The messages between the nodes of a distributed solver: the catalog, the workers and the clients, each known by its
 * address (see [Address]). On a machine the terms travel as they are (they are immutable, shared by the nodes
 * without copies); the transports between processes write them as Prolog text (see [MessageCodec]).
 *
 * After a change of the theory, the first evaluation builds the graph:
 * 1. the catalog sends each worker its rules ([Load]); each tells the premises they use ([Subscribe]);
 * 2. the catalog sends every worker the premises of all ([Build]); each builds the arguments of its rules, sends them
 *    to the workers whose rules use their conclusions ([Notify]), which build on them in turn, and publishes them to
 *    the catalog ([Publish]). Every Build, Notify and Publish is acknowledged to its sender ([Ack]); a worker
 *    acknowledges the message that engaged it only once all it sent is acknowledged (Dijkstra and Scholten): with
 *    every Build acknowledged, the arguments are all built and published;
 * 3. the workers find the attacks ([Attack]): each tells the others the statements that may attack its arguments
 *    ([Wanted]), sends each the arguments of its own for them ([Candidates]), finds the direct attacks on its own and
 *    sends them to the catalog ([AttacksFound]), which extends them to the arguments built on the attacked ones and
 *    labels the graph.
 */
sealed interface KbMessage

/** To the catalog: forget the theory; to a worker: forget its rules */
data object Reset : KbMessage

/** To the catalog: a graph being built lost some of its workers (a site left), build it again with those left */
data object Rebuild : KbMessage

/**
 * To the catalog: the whole theory, as Prolog text, partitioned among the workers (see TheoryPartitioner); to a
 * worker: its part of the theory, replacing the one it had
 */
data class Load(
    val theory: String,
) : KbMessage

/** To the catalog: a clause more for the theory, as Prolog text */
data class Add(
    val elem: String,
) : KbMessage

/** To the catalog: the labels of the arguments for the statement [elem], to send to the client [replyTo] */
data class RequireEvaluation(
    val elem: String,
    val replyTo: String,
) : KbMessage

/** To a client: the arguments for the statement it asked about, by label */
data class EvaluationResponse(
    val inArgs: List<Response>,
    val outArgs: List<Response>,
    val undArgs: List<Response>,
) : KbMessage

/** To the catalog: the statements the rules of [worker] use as premises, whose arguments it needs */
data class Subscribe(
    val worker: Int,
    val premises: List<Term>,
) : KbMessage

/** To a worker: build the arguments of its rules; [interests] are the premises of every worker */
data class Build(
    val interests: List<Interest>,
) : KbMessage

/** To a worker: arguments of the worker [from] for the premises of its rules, to build on */
data class Notify(
    val from: Int,
    val arguments: List<SharedArgument>,
) : KbMessage

/** To the catalog: the arguments [worker] built, with their supports */
data class Publish(
    val worker: Int,
    val arguments: List<SharedArgument>,
) : KbMessage

/** To the sender of a Build, a Notify or a Publish: it is handled, and so is all it led to; [from] is -1 for the catalog */
data class Ack(
    val from: Int,
) : KbMessage

/** To a worker: the arguments are all built, find the attacks on its own */
data object Attack : KbMessage

/** To another worker: the statements whose arguments may attack those of [worker] */
data class Wanted(
    val worker: Int,
    val statements: List<Term>,
) : KbMessage

/**
 * To another worker: the arguments of [worker] for the statements it wants, as much of them as an attack needs (their
 * top rules, conclusions, bodies and info, not their rules)
 */
data class Candidates(
    val worker: Int,
    val arguments: List<SharedArgument>,
) : KbMessage

/** To the catalog: the direct attacks on the arguments of [worker] */
data class AttacksFound(
    val worker: Int,
    val attacks: List<DirectAttack>,
) : KbMessage

/** The addresses of the nodes of a distributed solver */
object Address {
    /** The catalog, holding the theory, partitioning it and gathering the graph */
    const val CATALOG = "catalog"

    private const val WORKER = "worker_"

    /** The worker of the partition [index] */
    fun worker(index: Int) = "$WORKER$index"

    /** Whether [address] is the one of a worker */
    fun isWorker(address: String) = address.startsWith(WORKER)

    /** The index of the partition of the worker at [address] */
    fun indexOf(address: String) = address.removePrefix(WORKER).toInt()
}
