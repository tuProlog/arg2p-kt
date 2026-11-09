package it.unibo.tuprolog.argumentation.core.libraries.support.graphPrinter

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.support.graphPrinter.prolog.THEORY

/**
 * The printer of an argumentation graph and of its labels (module `graphPrinter`), for buildLabelSets/0: the arguments,
 * part by part, the supports and the attacks, then the arguments and the statements of each label.
 */
object GraphPrinterLibrary : PrologLibrary("graphPrinter") {
    override val prologRawTheory: String = THEORY
}
