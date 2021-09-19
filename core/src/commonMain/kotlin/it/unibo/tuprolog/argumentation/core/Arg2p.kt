package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.Debug
import it.unibo.tuprolog.argumentation.core.libs.DynamicLoader
import it.unibo.tuprolog.argumentation.core.libs.EngineInterface
import it.unibo.tuprolog.argumentation.core.libs.Utils
import it.unibo.tuprolog.solve.library.Libraries

val Arg2p = Libraries.of(
    EngineInterface,
    DynamicLoader,
    Utils,
    Debug
)
