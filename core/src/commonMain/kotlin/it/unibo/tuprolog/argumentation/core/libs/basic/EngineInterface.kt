package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.core.operators.OperatorSet

abstract class EngineInterfaceBase : PrologModule("prolog.argumentation.interface", "arg2p") {
    // The operators of the rules of a theory, for the theory of the interface and the solvers using it
    override val libraryOperators: OperatorSet
        get() = RuleParserBase.operators()

    override val theoryOperators: OperatorSet
        get() = super.theoryOperators.plus(RuleParserBase.operators())
}

expect object EngineInterface : EngineInterfaceBase {
    override val prologRawTheory: String
}
