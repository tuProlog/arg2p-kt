package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.ideal

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.ideal.prolog.THEORY

/**
 * The ideal labelling (module `ideal`, an argumentLabellingMode): the maximal admissible subset of the arguments IN
 * in every preferred labelling.
 */
object IdealLibrary : PrologLibrary("ideal") {
    override val prologRawTheory: String = THEORY
}
