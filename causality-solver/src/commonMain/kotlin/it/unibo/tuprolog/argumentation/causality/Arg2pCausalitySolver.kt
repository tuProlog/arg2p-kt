package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.causality.libs.CausalitySolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libs.basic.Cache
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.utils.Utils

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
