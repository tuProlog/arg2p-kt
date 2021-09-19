package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.Sources
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

object Debug : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.debug",
    theory = Sources.debug,
)
