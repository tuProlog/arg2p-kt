package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.Sources
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

object CompleteLabeller : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.graph.labelling.complete",
    theory = Sources.completeLabelling,
)
