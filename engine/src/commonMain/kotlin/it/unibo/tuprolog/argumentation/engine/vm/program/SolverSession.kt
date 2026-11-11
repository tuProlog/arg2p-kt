package it.unibo.tuprolog.argumentation.engine.vm.program

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.solver.VmSolver
import it.unibo.tuprolog.argumentation.engine.store.RecordIdentifiers
import it.unibo.tuprolog.argumentation.engine.words.locked
import it.unibo.tuprolog.solve.Solver

/** What a layer above the engine keeps in a [SolverSession], and its type */
@InternalArg2pApi
class SessionKey<T : Any>

/**
 * The solvers of one graph, on the threads of a process (the nodes of a cluster): their programs share one symbol
 * table (see [Symbols]), so that the records of one are records of every other as they are. A session has a base
 * program of its own, made from the base of the process as it is when the session begins, that compiles once for the
 * session the modules the base had not compiled then.
 *
 * Its symbols live as long as the session: a solver outside a session, as the most, has a table of its own.
 */
@InternalArg2pApi
class SolverSession {
    internal val base: Program = SharedPrograms.sessionBase().also { it.session = this }

    /**
     * The identifiers of the records of the contexts of the session (see `contextId/2`): the same argument the same
     * identifier on every node, one record however many build it. A node resetting its context keeps them: they go with
     * the session.
     */
    @InternalArg2pApi
    val identifiers = RecordIdentifiers(concurrent = true)

    // What the layers above the engine keep in the session (replaced as a whole, read by the threads of its solvers)
    private var kept: Map<SessionKey<*>, Any> = emptyMap()

    /** What the layers above the engine share among the solvers of the session under [key], if any */
    @Suppress("UNCHECKED_CAST")
    operator fun <T : Any> get(key: SessionKey<T>): T? = kept[key] as T?

    operator fun <T : Any> set(
        key: SessionKey<T>,
        value: T?,
    ) {
        // (the nodes of a session set their keys from their own threads: one write at a time, none lost)
        locked(this) { kept = if (value == null) kept - key else kept + (key to value) }
    }

    companion object {
        /** [solver] in [session], from its next resolution, if it is a solver of the Arg2P engine (others go on alone) */
        fun join(
            solver: Solver,
            session: SolverSession,
        ) {
            (solver as? VmSolver)?.join(session)
        }
    }
}
