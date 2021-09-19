package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.argumentation.core.Sources
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

object StructuredMode : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.structured",
    theory = Sources.queryMode,
)
