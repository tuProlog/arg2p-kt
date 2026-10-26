package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.message.Add
import it.unibo.tuprolog.argumentation.parallel.message.Address
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.KbMessage
import it.unibo.tuprolog.argumentation.parallel.message.Load
import it.unibo.tuprolog.argumentation.parallel.message.RequireEvaluation
import it.unibo.tuprolog.argumentation.parallel.message.Reset
import it.unibo.tuprolog.argumentation.parallel.node.Catalog
import it.unibo.tuprolog.argumentation.parallel.node.Network
import it.unibo.tuprolog.argumentation.parallel.node.Node
import it.unibo.tuprolog.argumentation.parallel.node.Worker

/**
 * A distributed solver whose nodes run in turn, on the thread of the caller: the same messages as on a cluster, in a
 * deterministic order, on every platform. The theory is split into at most [parts] partitions if given, one for each
 * group of rules supporting one another otherwise; the graph is labelled as the standard solver does with [settings].
 */
class LocalCluster(
    parts: Int? = null,
    settings: FlagsBuilder = FlagsBuilder(),
) {
    private val queue = ArrayDeque<Pair<String, KbMessage>>()
    private val network = Network { to, message -> queue.addLast(to to message) }
    private val nodes = mutableMapOf<String, Node>(Address.CATALOG to Catalog(network, parts, settings))
    private val responses = mutableMapOf<String, EvaluationResponse>()
    private var clients = 0

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = deliver(Address.CATALOG, Load(theory))

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = deliver(Address.CATALOG, Add(clause))

    /** Forgets the theory */
    fun reset() = deliver(Address.CATALOG, Reset)

    /** The arguments for [statement], as Prolog text, by label */
    fun solve(statement: String): EvaluationResponse {
        val client = "client_${clients++}"
        deliver(Address.CATALOG, RequireEvaluation(statement, client))
        return responses.remove(client) ?: error("No response for $statement")
    }

    // The message delivered, and those it causes in turn, until none is left
    private fun deliver(
        to: String,
        message: KbMessage,
    ) {
        queue.addLast(to to message)
        while (queue.isNotEmpty()) {
            val (address, next) = queue.removeFirst()
            when {
                Address.isWorker(address) -> nodes.getOrPut(address) { Worker(address, network) }.receive(next)
                next is EvaluationResponse -> responses[address] = next
                else -> nodes[address]?.receive(next)
            }
        }
    }
}
