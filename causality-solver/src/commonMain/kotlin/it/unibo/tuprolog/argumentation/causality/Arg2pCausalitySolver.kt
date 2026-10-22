package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.causality.libs.CausalitySolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.cache.Cache
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.modules.language.RuleParser
import it.unibo.tuprolog.argumentation.core.modules.utils.Utils

fun Arg2pSolverFactory.causality(theory: String = "") =
    of(
        theory,
        FlagsBuilder().create(),
        listOf(CausalitySolver(), Cache()),
        listOf(
            Utils,
            RuleParser,
        ),
    )
