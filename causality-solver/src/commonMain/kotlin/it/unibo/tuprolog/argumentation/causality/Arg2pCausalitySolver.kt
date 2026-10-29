package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.causality.modules.CausalityModule
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.modules.ModuleRegistry
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder

/**
 * A solver of [theory] with the causality module (see [CausalityModule]), its predicates called without the module:
 * the other modules are loaded by their first call (the parser, and the modules it relies on), and the cache is the
 * one of every solver
 */
fun Arg2pSolverFactory.causality(theory: String = "") =
    of(
        theory,
        FlagsBuilder().create(),
        listOf(CausalityModule()),
        ModuleRegistry.all,
    )
