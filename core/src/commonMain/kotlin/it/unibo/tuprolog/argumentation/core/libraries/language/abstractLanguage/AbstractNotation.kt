package it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier

/** The operators of the notation of the abstract frameworks */
@InternalArg2pApi
object AbstractNotation {
    /** The notation of the abstract frameworks, beyond the default operators: `a <-> b` */
    val operators: OperatorSet = OperatorSet(Operator("<->", Specifier.XFX, 1050))
}
