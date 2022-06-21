package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

abstract class StructuredModeBase : ArgLibrary, LazyRawPrologContent(), Loadable {

    override val alias = "prolog.argumentation.structured"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(QueryMode, AmbiguityBlocking)

    override fun identifier(): String = "structured"

    override val theoryOperators = DynamicLoader.operators()
        .plus(OperatorSet.DEFAULT)
}

expect object StructuredMode : StructuredModeBase

object QueryMode : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "queryMode"
    override fun default(): Boolean = true
    override fun values() {}
}

object AmbiguityBlocking : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "ambiguityBlocking"
    override fun default(): Boolean = true
    override fun values() {}
}
