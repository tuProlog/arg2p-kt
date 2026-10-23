package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.causality.libs.CausalitySolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.modules.ModuleRegistry
import it.unibo.tuprolog.argumentation.core.runtime.cache.Cache
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder

// The modules are loaded by their first call: the parser, and the modules it relies on (the rules, transposition)
fun Arg2pSolverFactory.causality(theory: String = "") =
    of(
        theory,
        FlagsBuilder().create(),
        listOf(CausalitySolver(), Cache()),
        ModuleRegistry.all,
    )
