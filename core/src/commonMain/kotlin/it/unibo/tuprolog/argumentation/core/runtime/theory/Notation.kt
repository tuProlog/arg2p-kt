package it.unibo.tuprolog.argumentation.core.runtime.theory

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier

/**
 * The operators the theories of arg2p are written with. Those of the notations of all the profiles are those of every
 * theory, none conflicting with another: each profile reads the clauses written with them its own way.
 */
@InternalArg2pApi
object Notation {
    /** The module calls: `Module::Goal`, `Module:::Goal` */
    val moduleCalls: OperatorSet =
        OperatorSet(
            Operator("::", Specifier.XFX, 700),
            Operator(":::", Specifier.XFX, 700),
        )

    /** The notation of the abstract frameworks, beyond the default operators: `a <-> b` */
    val abstractFramework: OperatorSet = OperatorSet(Operator("<->", Specifier.XFX, 1050))

    /** The rules of a theory: `r : a => b`, `f :=> a`, `f :-> a`, and the notation of the abstract frameworks */
    val rules: OperatorSet =
        OperatorSet(
            Operator("=>", Specifier.XFX, 1199),
            Operator(":=>", Specifier.XFX, 1199),
            Operator(":->", Specifier.XFX, 1199),
            Operator(":", Specifier.XFX, 1001),
        ) + abstractFramework

    /** The theories of the modules: the module calls and the default operators */
    val modules: OperatorSet = moduleCalls.plus(OperatorSet.DEFAULT)

    /** The theories of arg2p: the theories of the modules and the rules */
    val theories: OperatorSet = modules.plus(rules)
}
