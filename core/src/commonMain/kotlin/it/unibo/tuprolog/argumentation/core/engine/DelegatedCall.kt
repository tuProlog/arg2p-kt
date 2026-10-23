package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * A primitive whose goal the engine can solve itself, as a sub-goal of the same resolution, rather than through the
 * solutions the primitive computes (arg2p: module calls, context lookups). Other solvers just call [solve].
 */
interface DelegatedCall : Primitive {
    /** How the engine solves the call [request]; null when it is solved as any primitive, through [solve] */
    fun delegate(request: Solve.Request<ExecutionContext>): Delegation?
}

/** How the engine solves a [DelegatedCall], in a frame of its own opaque to cut (as call/1) */
sealed interface Delegation

/**
 * The goal solved in place, against the [libraries] given: when the frame is left, the caller goes on with its own
 * libraries (module calls)
 */
data class Inlining(
    val goal: Struct,
    val libraries: Runtime,
) : Delegation

/**
 * The goal (not a conjunction) resolved with the clauses of [source] as the clauses of its predicate, read when the
 * call is made (context lookups)
 */
data class Sourcing(
    val goal: Struct,
    val source: ClauseSource,
) : Delegation
