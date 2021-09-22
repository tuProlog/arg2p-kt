package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory

interface ArgLibrary {
    val baseContent : AliasedLibrary
    val baseFlags : Iterable<ArgsFlag<*, *>>

    fun flags() = baseFlags
    fun content() = baseContent
}

interface UnionArgLibrary<T> : ArgLibrary where T : ArgLibrary  {

    val parentLibrary: T

    override fun flags() = baseFlags.union(parentLibrary.flags())
    override fun content() = Library.aliased(
        alias = baseContent.alias,
        theory = baseContent.theory.plus(parentLibrary.content().theory)
    )
}

interface RawPrologContent {
    val prologTheory: Theory
}

interface Loadable {
    fun identifier(): String
}

interface ArgsFlag<T, G> {
    fun predicate() : String
    fun default() : T
    fun values() : G
}
