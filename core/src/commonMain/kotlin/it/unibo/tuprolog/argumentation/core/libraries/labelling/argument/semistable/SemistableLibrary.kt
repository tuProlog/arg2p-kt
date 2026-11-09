package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.semistable

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.semistable.prolog.THEORY

/**
 * The semi-stable labellings (module `semistable`, an argumentLabellingMode): the preferred ones with a minimal set
 * of UND arguments.
 */
object SemistableLibrary : PrologLibrary("semistable") {
    override val prologRawTheory: String = THEORY
}
