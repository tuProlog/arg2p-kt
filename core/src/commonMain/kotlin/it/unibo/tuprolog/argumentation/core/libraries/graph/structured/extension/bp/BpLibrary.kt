package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.bp

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.bp.prolog.THEORY

/**
 * The burdens of proof as part of the graph (module `bp`, a graphExtension): for an argument for `bp(Statement)` and
 * an argument for Statement, the burdened one, an argument for `-burdmet(Conclusion)` (top rule `artificial`, one for
 * each of the two) is attacked as the burden of proof is, and rebuts or is rebutted by the burdened argument,
 * depending on whether the grounded labelling labels it IN.
 */
object BpLibrary : PrologLibrary("bp") {
    override val prologRawTheory: String = THEORY
}
