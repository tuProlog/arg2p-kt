package it.unibo.tuprolog.argumentation.engine.solver

import it.unibo.tuprolog.argumentation.engine.vm.Machine
import it.unibo.tuprolog.solve.AbstractSolutionStream
import it.unibo.tuprolog.solve.Solution
import kotlin.concurrent.Volatile

/**
 * The solutions of a goal on [machine], as tuProlog's solvers give them: [cancel] stops the resolution (from any thread: the
 * machine reads it within a few calls, and its next solution is a halt, the last), [close] stops it and gives the memory
 * of the machine back once it is not running. Iterated once.
 */
internal class MachineSolutionStream(
    private val solutions: Iterator<Solution>,
    private val machine: Machine,
) : AbstractSolutionStream() {
    @Volatile private var cancelled = false

    private var closed = false

    // whether a solution is being looked for (the machine then gives its memory back itself, once it stops)
    private var running = false

    override val isCancelled: Boolean
        get() = cancelled

    override fun hasNextSolution(): Boolean = !closed && solutions.hasNext()

    override fun nextSolution(): Solution {
        if (closed) throw NoSuchElementException()
        running = true
        try {
            return solutions.next()
        } finally {
            running = false
        }
    }

    override fun cancel() {
        cancelled = true
        machine.cancelled = true
    }

    override fun close() {
        if (closed) return
        closed = true
        cancel()
        if (!running) machine.release()
    }
}
