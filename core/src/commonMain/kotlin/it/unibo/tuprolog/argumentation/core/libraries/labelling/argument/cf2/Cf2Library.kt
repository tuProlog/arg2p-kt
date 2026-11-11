package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.cf2

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.cf2.prolog.THEORY

/**
 * The cf2 labellings (module `cf2`, an argumentLabellingMode): the naive labellings of each strongly connected
 * component, the components taken in turn once the arguments attacked by the previous ones are suppressed.
 */
object Cf2Library : PrologLibrary("cf2") {
    override val prologRawTheory: String = THEORY
}
