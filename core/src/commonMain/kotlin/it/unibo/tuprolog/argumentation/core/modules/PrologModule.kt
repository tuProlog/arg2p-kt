package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive

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
