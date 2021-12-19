package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library

sealed class EngineInterfaceBase : ArgLibrary, LazyRawPrologContent() {

    override val alias = "prolog.argumentation.interface"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = this.prologTheory,
            operatorSet = RuleParserBase.operators()
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override val theoryOperators = RuleParserBase.operators()
        .plus(DynamicLoader.operators())
        .plus(OperatorSet.DEFAULT)
}

expect object EngineInterface : EngineInterfaceBase
