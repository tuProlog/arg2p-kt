package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentBuilder

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentBuilder.prolog.THEORY

/**
 * The arguments of a theory (module `argumentBuilder`): every argument its premises and rules make, built round after
 * round from the conclusions of the previous one, and saved in the context with their supports (see the module
 * store).
 */
object ArgumentBuilderLibrary : PrologLibrary("argumentBuilder") {
    override val prologRawTheory: String = THEORY
}
