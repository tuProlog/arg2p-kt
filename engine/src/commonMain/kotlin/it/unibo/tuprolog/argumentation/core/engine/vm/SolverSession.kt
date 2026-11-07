package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.solve.Solver

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
