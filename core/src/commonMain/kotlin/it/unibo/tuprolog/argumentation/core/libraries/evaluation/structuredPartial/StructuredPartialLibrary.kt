package it.unibo.tuprolog.argumentation.core.libraries.evaluation.structuredPartial

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.structuredPartial.prolog.THEORY

/**
 * The evaluation of the part of the argumentation graph the labels of the statements for a goal depend on (module
 * `structuredPartial`), as the locality of the semantics goes (see locality/1): the part explored from the goal (see the module
 * queryExploration), then changed and labelled as the whole graph is (see the module phases). The solvers of a session
 * building it together (see GraphSharing) explore it each with its rules, through the graph store.
 */
object StructuredPartialLibrary : PrologLibrary("structuredPartial") {
    override val prologRawTheory: String = THEORY
}
