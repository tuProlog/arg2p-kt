package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.admissible

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.admissible.prolog.THEORY

/**
 * The admissible labellings (module `admissible`, an argumentLabellingMode): the conflict-free ones in which
 * every attacker of an IN argument is attacked by an IN argument.
 */
object AdmissibleLibrary : PrologLibrary("admissible") {
    override val prologRawTheory: String = THEORY
}
