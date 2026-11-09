package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stable

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stable.prolog.THEORY

/**
 * The stable labellings (module `stable`, an argumentLabellingMode): those leaving no argument UND, built one
 * strongly connected component at a time (see the module scc).
 */
object StableLibrary : PrologLibrary("stable") {
    override val prologRawTheory: String = THEORY
}
