package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.preferred

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.preferred.prolog.THEORY

/**
 * The preferred labellings (module `preferred`, an argumentLabellingMode): the admissible ones with a maximal set of
 * IN arguments, built one strongly connected component at a time (see the module scc).
 */
object PreferredLibrary : PrologLibrary("preferred") {
    override val prologRawTheory: String = THEORY
}
