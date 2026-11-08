package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.argumentation.core.runtime.modules.Loadable
import it.unibo.tuprolog.argumentation.core.runtime.theory.Notation
import it.unibo.tuprolog.argumentation.engine.vm.CodeKey
import it.unibo.tuprolog.argumentation.engine.vm.SharedLibrary
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

/**
 * A module of arg2p, loaded by its first call (Module::Goal): a Prolog theory ([prologRawTheory], parsed once with
 * the [theoryOperators] of the modules), with the native [primitives] it relies on and the [baseFlags] it declares,
 * under an [alias] and the [moduleIdentifier] the calls name it by. Its library is built once and shared by every
 * solver: the native primitives of a module keep no state.
 */
abstract class PrologModule(
    final override val alias: String,
    private val moduleIdentifier: String,
) : Loadable {
    /** The Prolog theory of the module, as text */
    abstract val prologRawTheory: String

    /** The theory of the module, parsed once with the operators of the modules */
    val prologTheory: Theory by lazy { Theory.parse(prologRawTheory, theoryOperators) }

    /** The native primitives of the module */
    protected open val primitives: Map<Signature, Primitive>
        get() = emptyMap()

    /** The operators the library of the module gives the solvers using it */
    protected open val libraryOperators: OperatorSet
        get() = OperatorSet()

    override val baseContent: Library by lazy {
        SharedLibrary(
            Library.of(alias = alias, clauses = prologTheory, primitives = primitives, operators = libraryOperators),
            CodeKey.of(alias, prologRawTheory),
        )
    }

    override val theoryOperators: OperatorSet
        get() = Notation.modules

    final override fun identifier(): String = moduleIdentifier
}
