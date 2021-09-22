package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class EngineInterfaceBase : ArgLibrary, RawPrologContent {

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = "prolog.argumentation.interface",
            theory = this.prologTheory,
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}


expect object EngineInterface : EngineInterfaceBase
