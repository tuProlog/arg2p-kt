package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

interface ArgContext : ArgLibrary
interface ArgLoader : ArgLibrary

interface ArgLibrary {
    val alias: String
    val baseContent: AliasedLibrary
    val baseFlags: Iterable<ArgsFlag<*, *>>
    var theoryOperators: OperatorSet

    fun flags() = baseFlags
    fun content() = baseContent
}

abstract class BaseArgLibrary : ArgLibrary {
    override var theoryOperators: OperatorSet = OperatorSet.DEFAULT
        get() = OperatorSet.DEFAULT.plus(if (field == OperatorSet.DEFAULT) field else OperatorSet.DEFAULT.plus(field))
}

interface UnionArgLibrary<T> : ArgLibrary where T : ArgLibrary {

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

abstract class LazyRawPrologContent : BaseArgLibrary(), RawPrologContent {
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
