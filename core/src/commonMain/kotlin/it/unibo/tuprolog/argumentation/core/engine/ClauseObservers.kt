package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.core.Clause

/** Told of each clause the engine resolves a goal with (arg2p): to measure which clauses a run uses */
fun interface ClauseObserver {
    fun resolved(clause: Clause)
}

/**
 * The observers of the clauses resolved by the engine, in any resolution of any solver while they are registered: an
 * instrument for tools such as coverage reports, costing a check of an empty list otherwise.
 */
object ClauseObservers {
    private var observers: List<ClauseObserver> = emptyList()

    /** Registers [observer] until the returned handle is closed */
    fun register(observer: ClauseObserver): AutoCloseable {
        observers = observers + observer
        return AutoCloseable { observers = observers - observer }
    }

    internal fun resolved(clause: Clause) {
        val current = observers
        if (current.isEmpty()) return
        current.forEach { it.resolved(clause) }
    }
}
