package it.unibo.tuprolog.argumentation.core.libraries.graph.abstractGraph

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.abstractGraph.prolog.THEORY

/**
 * The argumentation graph of an abstract theory (module `abstractGraph`, the graphBuilder of the profile abstract): the
 * arguments and the attacks the abstract language reads of it (see abstractLanguage::arguments/1, attacks/1); an argument
 * attacking or attacked is an argument as well, each concluding its own name (so the statements are the arguments).
 */
internal object AbstractFrameworkGraphLibrary : PrologLibrary("abstractGraph") {
    override val prologRawTheory: String = THEORY
}
