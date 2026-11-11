package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.rebutRestriction

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.rebutRestriction.prolog.THEORY

/**
 * The restricted rebut (module `rebutRestriction`, a graphExtension): a rebut only succeeds on an argument whose top
 * rule is defeasible.
 */
object RebutRestrictionLibrary : PrologLibrary("rebutRestriction") {
    override val prologRawTheory: String = THEORY
}
