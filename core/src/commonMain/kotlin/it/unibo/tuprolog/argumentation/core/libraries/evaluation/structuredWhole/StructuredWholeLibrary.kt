package it.unibo.tuprolog.argumentation.core.libraries.evaluation.structuredWhole

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.structuredWhole.prolog.THEORY

/**
 * The evaluation of the whole argumentation graph of a theory (module `structuredWhole`): the graph built by the builder of its
 * profile, then changed and labelled (see the module phases).
 */
object StructuredWholeLibrary : PrologLibrary("structuredWhole") {
    override val prologRawTheory: String = THEORY
}
