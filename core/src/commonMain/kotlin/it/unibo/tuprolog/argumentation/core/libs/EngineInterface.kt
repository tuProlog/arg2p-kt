package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class EngineInterfaceBase : ArgLibrary, LazyRawPrologContent() {

    override val alias = "prolog.argumentation.interface"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = this.prologTheory,
            operatorSet = OperatorSet(
                Operator("=>", Specifier.XFX, 1199),
                Operator(":=>", Specifier.XFX, 1199),
                Operator(":->", Specifier.XFX, 1199),
                Operator(":", Specifier.XFX, 1001),
                Operator(":=", Specifier.XFX, 1199)
            )
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}

expect object EngineInterface : EngineInterfaceBase
