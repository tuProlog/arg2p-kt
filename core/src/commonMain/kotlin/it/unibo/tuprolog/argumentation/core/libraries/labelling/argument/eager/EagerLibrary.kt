package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.eager

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.eager.prolog.THEORY

/**
 * The eager labelling (module `eager`, an argumentLabellingMode): the maximal admissible subset of the arguments IN
 * in every semi-stable labelling.
 */
object EagerLibrary : PrologLibrary("eager") {
    override val prologRawTheory: String = THEORY
}
