@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import kotlin.js.JsExport

/**
 * A distributed solver, on any platform (see the parallel solvers of each one): its theory, its flags, and the last evaluation it
 * answered. To build its graphs with other instances as well, see the module federation.
 */
@JsExport
abstract class ParallelSolver internal constructor(
    // the node of the parallel solver, here or on another thread
    internal val access: NodeAccess,
) {
    /**
     * The flags of the solver, the initial ones those given: those set apply from the next evaluation on, the graph
     * built anew
     */
    var flags: Flags
        get() = access.flags
        set(value) {
            access.flags = value
        }

    /** Replaces the theory with [theory], as Prolog text */
    fun load(theory: String) = access.load(theory)

    /** Adds [clause], as Prolog text, to the theory */
    fun add(clause: String) = access.add(clause)

    /** Forgets the theory */
    fun reset() = access.reset()

    /** The last evaluation the parallel solver answered, none before the first */
    val evaluation: EvaluationStats? get() = access.evaluation
}
