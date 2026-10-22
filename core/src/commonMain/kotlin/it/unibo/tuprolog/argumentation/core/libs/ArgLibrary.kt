package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

// primitives: Map<Signature, Primitive> = emptyMap(),
// clauses: Iterable<Clause> = emptyList(),
// operators: OperatorSet = OperatorSet(),
// functions: Map<Signature, LogicFunction> = emptyMap()

fun Theory.toLibrary(alias: String): Library = Library.of(alias, clauses = this.clauses)

fun Library.toTheory(): Theory = Theory.of(this.clauses)

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

// interface UnionArgLibrary<T> : ArgLibrary where T : ArgLibrary {
//
//    val parentLibrary: T
//
//    override fun flags() = baseFlags.union(parentLibrary.flags())
//    override fun content() = Library.of(
//        alias = baseContent.alias,
//        theory = baseContent.theory.plus(parentLibrary.content().theory)
//    )
// }

interface RawPrologContent {
    val prologTheory: Theory
}

abstract class LazyRawPrologContent :
    ArgLibrary,
    RawPrologContent {
    abstract val prologRawTheory: String
    override val prologTheory: Theory by lazy {
        Theory.parse(prologRawTheory, theoryOperators)
    }
}

interface Loadable {
    fun identifier(): String
}

/**
 * A flag of arg2p, declared by the library it configures and checked through setting/1,2 (see
 * [it.unibo.tuprolog.argumentation.core.libs.basic.Settings]): a theory states its values as facts of its [predicate]
 */
interface ArgsFlag<T, G> {
    fun predicate(): String

    fun default(): T

    fun values(): G

    /** Whether the flag is a switch, on when stated by its predicate alone (`queryMode.`), rather than taking values */
    val isSwitch: Boolean
        get() = false
}

interface PrimitiveWithSignature : Primitive {
    val signature: Signature
}
