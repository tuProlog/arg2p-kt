package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

abstract class DebugBase : ArgLibrary, LazyRawPrologContent(), Loadable {

    override val alias = "prolog.argumentation.debug"

    override val baseContent: Library
        get() = Library.of(
            alias = this.alias,
            clauses = this.prologTheory
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier() = "debug"

    override val theoryOperators = DynamicLoader.operators()
        .plus(OperatorSet.DEFAULT)
}

expect object Debug : DebugBase
