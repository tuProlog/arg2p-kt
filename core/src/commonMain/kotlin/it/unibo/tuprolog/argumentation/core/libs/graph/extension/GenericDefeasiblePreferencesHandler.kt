package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.Sources
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

object GenericDefeasiblePreferencesHandler : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.graph.preferences.defeasible.generic",
    theory = Sources.genericDefPreferences,
)
