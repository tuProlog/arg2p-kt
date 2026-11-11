package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stage

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stage.prolog.THEORY

/**
 * The stage labellings (module `stage`, an argumentLabellingMode): the naive ones with a minimal set of UND
 * arguments.
 */
object StageLibrary : PrologLibrary("stage") {
    override val prologRawTheory: String = THEORY
}
