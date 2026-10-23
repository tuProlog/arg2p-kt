package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

/**
 * A part of arg2p a solver is built from (a module, the context, the loader): its content, the flags it declares, and
 * the operators of its theory
 */
interface ArgLibrary {
    val alias: String
    val baseContent: Library
    val baseFlags: Iterable<ArgsFlag<*, *>>

    val theoryOperators: OperatorSet
        get() = OperatorSet.DEFAULT

    fun flags() = baseFlags

    fun content() = baseContent
}
