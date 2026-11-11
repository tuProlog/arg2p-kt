package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.semiStable

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.semiStable.prolog.THEORY

/**
 * The semi-stable labellings (module `semiStable`, an argumentLabellingMode): the preferred ones with a minimal set
 * of UND arguments.
 */
object SemiStableLibrary : PrologLibrary("semiStable") {
    override val prologRawTheory: String = THEORY
}
