package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.*
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class GroundedLabellerBase : ArgLibrary, LazyRawPrologContent(), Loadable {

    override val alias = "prolog.argumentation.graph.labelling.grounded"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "grounded"
}

expect object GroundedLabeller : GroundedLabellerBase
