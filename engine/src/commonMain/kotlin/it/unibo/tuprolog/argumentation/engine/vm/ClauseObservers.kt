package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.core.Clause

/** Told of each clause the machine resolves a goal with: to measure which clauses a run uses */
fun interface ClauseObserver {
    fun resolved(clause: Clause)
}

/**
 * The observers of the clauses resolved by the machine, in any resolution of any solver while they are registered: an
 * instrument for tools such as coverage reports, costing a check of an empty list otherwise.
 */
object ClauseObservers {
    private var observers: List<ClauseObserver> = emptyList()

    /** Whether any observer is registered (the machine asks before telling) */
    var active = false
        private set

    /** Registers [observer] until the returned handle is closed */
    fun register(observer: ClauseObserver): AutoCloseable {
        observers = observers + observer
        active = true
        return AutoCloseable {
            observers = observers - observer
            active = observers.isNotEmpty()
        }
    }

    fun resolved(clause: Clause) {
        val current = observers
        if (current.isEmpty()) return
        current.forEach { it.resolved(clause) }
    }
}
