package it.unibo.tuprolog.argumentation.core.libraries.arg2p

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.arg2p.prolog.THEORY

/**
 * The entry points of arg2p (module `arg2p`, loaded by every solver): the labels of the statements of the theory,
 * of the whole graph or of a query: solve/0..4. buildLabelSets/0,2,3, solve/0 and solve/1,2,4 are
 * the entry points of earlier versions, kept for what was written for them, calling solve.
 */
object Arg2pInterfaceLibrary : PrologLibrary("arg2p") {
    override val prologRawTheory: String = THEORY
}
