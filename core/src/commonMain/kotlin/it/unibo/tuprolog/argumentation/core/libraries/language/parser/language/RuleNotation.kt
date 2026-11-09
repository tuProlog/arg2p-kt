package it.unibo.tuprolog.argumentation.core.libraries.language.parser.language

import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation.ModuleNotation
import it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage.AbstractNotation
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier

/**
 * The operators the theories of arg2p are written with. Those of the notations of all the profiles are those of every
 * theory, none conflicting with another: each profile reads the clauses written with them its own way.
 */
@InternalArg2pApi
object RuleNotation {
    /** The rules of a theory: `r : a => b`, `f :=> a`, `f :-> a`, and the notation of the abstract frameworks */
    val rules: OperatorSet =
        OperatorSet(
            Operator("=>", Specifier.XFX, 1199),
            Operator(":=>", Specifier.XFX, 1199),
            Operator(":->", Specifier.XFX, 1199),
            Operator(":", Specifier.XFX, 1001),
        ) + AbstractNotation.operators

    /** The theories of arg2p: the theories of the modules and the rules */
    val theories: OperatorSet = ModuleNotation.theories.plus(rules)
}
