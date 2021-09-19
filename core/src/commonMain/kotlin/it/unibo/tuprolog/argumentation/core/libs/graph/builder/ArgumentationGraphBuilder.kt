package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.argumentation.core.Sources
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

object ArgumentationGraphBuilder : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.graph",
    theory = Sources.argumentationGraph,
)
