package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stage2

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stage2.prolog.THEORY

/**
 * The stage2 labellings (module `stage2`, an argumentLabellingMode): as cf2, with the stage labellings in each
 * strongly connected component.
 */
object Stage2Library : PrologLibrary("stage2") {
    override val prologRawTheory: String = THEORY
}
