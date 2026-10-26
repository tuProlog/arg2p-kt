package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.parallel.message.KbMessage

/**
 * Delivers the messages between the nodes of a distributed solver, by their addresses (see
 * [it.unibo.tuprolog.argumentation.parallel.message.Address]): the messages from a node to another one arrive in the
 * order they were sent
 */
fun interface Network {
    fun send(
        to: String,
        message: KbMessage,
    )

    /** The workers the network can run, if it limits them: the catalog splits the theory into at most as many parts */
    val capacity: Int? get() = null

    /**
     * The catalog is about to build a graph with [workers] workers: the messages to the workers of a graph built
     * before are no longer delivered
     */
    fun building(workers: Int) = Unit
}

/** A node of a distributed solver (the coordinator, a worker): it handles the messages sent to it, one at a time */
fun interface Node {
    fun receive(message: KbMessage)
}
