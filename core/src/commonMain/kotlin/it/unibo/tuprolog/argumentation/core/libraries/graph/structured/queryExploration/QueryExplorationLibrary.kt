package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.queryExploration

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.queryExploration.primitives.GroundVars
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.queryExploration.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The part of the argumentation graph a query depends on (module `queryExploration`, for query mode): the arguments
 * for the query, and, recursively, those their labels may depend on, saved as they are built (see the module
 * argumentSearch).
 */
object QueryExplorationLibrary : PrologLibrary("queryExploration") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(GroundVars.signature to GroundVars)

    override val prologRawTheory: String = THEORY
}
