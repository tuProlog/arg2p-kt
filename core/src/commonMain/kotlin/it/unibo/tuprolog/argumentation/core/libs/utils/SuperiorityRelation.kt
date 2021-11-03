package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class SuperiorityRelationBase : ArgLibrary, LazyRawPrologContent(), Loadable {

    override val alias = "prolog.argumentation.superiority"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(OrderingPrinciple, OrderingComparator)

    override fun identifier(): String = "superiority"
}

expect object SuperiorityRelation : SuperiorityRelationBase

object OrderingPrinciple : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingPrinciple"
    override fun default(): String = "last"
    override fun values(): Iterable<String> = listOf("last", "weakest")
}

object OrderingComparator : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingComparator"
    override fun default(): String = "elitist"
    override fun values(): Iterable<String> = listOf("elitist", "democrat", "normal")
}
