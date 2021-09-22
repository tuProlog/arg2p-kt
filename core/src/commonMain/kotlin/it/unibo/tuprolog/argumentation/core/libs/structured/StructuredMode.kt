package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.argumentation.core.libs.*
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library


sealed class StructuredModeBase : ArgLibrary, LazyRawPrologContent(), Loadable {

    override val alias = "prolog.argumentation.structured"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(QueryMode)

    override fun identifier(): String = "structured"
}

expect object StructuredMode : StructuredModeBase

object QueryMode : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "queryMode"
    override fun default(): Boolean = true
    override fun values() {}
}
