package it.unibo.tuprolog.argumentation.actor

/** A cluster that cannot answer: none joined, or no answer in time */
internal class ClusterException(
    message: String,
) : RuntimeException(message)
