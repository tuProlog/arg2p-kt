package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory

/**
 * A solver of [theory] with the causality module (see [CausalityLibrary]), its predicates called without the module:
 * the other modules are loaded by their first call (the parser, and the modules it relies on), and the cache is the
 * one of every solver
 */
fun Arg2pSolverFactory.causality(theory: String = "") = default(theory, Flags.defaults().library(), listOf(CausalityLibrary()))
