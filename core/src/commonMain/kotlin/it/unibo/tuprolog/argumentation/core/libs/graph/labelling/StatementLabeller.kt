package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.*
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class StatementLabellerBase : ArgLibrary, LazyRawPrologContent(), Loadable {
    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = "prolog.argumentation.graph.labelling.statement",
            theory = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "statement"
}

expect object StatementLabeller : StatementLabellerBase
