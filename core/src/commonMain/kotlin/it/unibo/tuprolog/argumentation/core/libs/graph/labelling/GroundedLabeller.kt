package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.RawPrologContent
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class GroundedLabellerBase : ArgLibrary, RawPrologContent, Loadable {
    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = "prolog.argumentation.graph.labelling.grounded",
            theory = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "grounded"
}

expect object GroundedLabeller : GroundedLabellerBase
