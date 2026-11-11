package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.complete

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.complete.prolog.THEORY

/**
 * The complete labellings (module `complete`, an argumentLabellingMode): the admissible ones labelling IN every
 * argument they defend, built one strongly connected component at a time (see the module scc).
 */
object CompleteLibrary : PrologLibrary("complete") {
    override val prologRawTheory: String = THEORY
}
