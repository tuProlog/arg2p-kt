package it.unibo.tuprolog.argumentation.core.libraries.evaluation.abstract

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.abstract.prolog.THEORY

/**
 * The labelling of abstract argumentation frameworks (module `abstract`): arguments and attacks given as they are,
 * labelled by the semantics of argumentLabellingMode.
 */
object AbstractFrameworkLibrary : PrologLibrary("abstract") {
    override val prologRawTheory: String = THEORY
}
