package it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier

/** The operators of the module calls, and those the theories of the modules are written with */
@InternalArg2pApi
object ModuleNotation {
    /** The module calls: `Module::Goal`, `Module:::Goal` */
    val calls: OperatorSet =
        OperatorSet(
            Operator("::", Specifier.XFX, 700),
            Operator(":::", Specifier.XFX, 700),
        )

    /** The theories of the modules: the module calls and the default operators */
    val theories: OperatorSet = calls.plus(OperatorSet.DEFAULT)
}
