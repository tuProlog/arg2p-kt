package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * A primitive whose goal the engine solves in place: as a sub-goal of the same resolution, against other libraries,
 * rather than in a solver of its own (arg2p: the module calls). Other solvers just call [solve].
 *
 * The inlined goal runs in a frame of its own, opaque to cut (as call/1), whose libraries are the ones [inline]
 * tells: when the frame is left, the caller goes on with its own libraries.
 */
interface InlinedCall : Primitive {
    fun inline(request: Solve.Request<ExecutionContext>): Inlining
}

/** The goal an [InlinedCall] stands for, and the libraries to solve it against */
data class Inlining(
    val goal: Struct,
    val libraries: Runtime,
)
