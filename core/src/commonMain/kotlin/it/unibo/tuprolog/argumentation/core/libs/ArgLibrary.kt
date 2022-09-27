package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

interface ArgContext : ArgLibrary
interface ArgLoader : ArgLibrary

interface ArgLibrary {
    val alias: String
    val baseContent: Library
    val baseFlags: Iterable<ArgsFlag<*, *>>

    val theoryOperators: OperatorSet
        get() = OperatorSet.DEFAULT

    fun flags() = baseFlags
    fun content() = baseContent
}

interface UnionArgLibrary<T> : ArgLibrary where T : ArgLibrary {

    val parentLibrary: T

    override fun flags() = baseFlags.union(parentLibrary.flags())
    override fun content() = Library.of(
        alias = baseContent.alias,
        theory = baseContent.theory.plus(parentLibrary.content().theory)
    )
}

interface RawPrologContent {
    val prologTheory: Theory
}

abstract class LazyRawPrologContent : ArgLibrary, RawPrologContent {
    abstract val prologRawTheory: String
    override val prologTheory: Theory by lazy {
        Theory.parse(prologRawTheory, theoryOperators)
    }
}

interface Loadable {
    fun identifier(): String
}

interface ArgsFlag<T, G> {
    fun predicate(): String
    fun default(): T
    fun values(): G
}

interface PrimitiveWithSignature : Primitive {
    val signature: Signature
}
