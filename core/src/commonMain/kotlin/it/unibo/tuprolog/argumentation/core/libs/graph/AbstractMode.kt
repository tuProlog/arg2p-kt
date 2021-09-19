package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.argumentation.core.Sources
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

object AbstractMode : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.abstract",
    theory = Sources.abstractMode,
)
