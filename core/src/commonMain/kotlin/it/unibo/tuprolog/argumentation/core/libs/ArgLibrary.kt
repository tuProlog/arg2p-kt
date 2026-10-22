package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
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

/**
 * A module of arg2p, loaded by its first call (Module::Goal): a Prolog theory ([prologRawTheory], parsed once with
 * the [theoryOperators] of the modules), with the native [primitives] it relies on and the [baseFlags] it declares,
 * under an [alias] and the [moduleIdentifier] the calls name it by. Its library is built once and shared by every
 * solver: the native primitives of a module keep no state.
 */
abstract class PrologModule(
    final override val alias: String,
    private val moduleIdentifier: String,
) : LazyRawPrologContent(),
    Loadable {
    /** The native primitives of the module */
    protected open val primitives: Map<Signature, Primitive>
        get() = emptyMap()

    /** The operators the library of the module gives the solvers using it */
    protected open val libraryOperators: OperatorSet
        get() = OperatorSet()

    override val baseContent: Library by lazy {
        Library.of(alias = alias, clauses = prologTheory, primitives = primitives, operators = libraryOperators)
    }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override val theoryOperators: OperatorSet
        get() = MODULE_OPERATORS

    final override fun identifier(): String = moduleIdentifier

    private companion object {
        // The operators of the theories of the modules: those of module calls (::, :::) and the default ones
        val MODULE_OPERATORS: OperatorSet by lazy { DynamicLoader.operators().plus(OperatorSet.DEFAULT) }
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
