package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.subsumes
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.reply
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.solve.Signature

/**
 * subsumes(+General, +Specific): Specific is an instance of General (subsumes_term/2, which the solver lacks): General
 * unifies with Specific, the variables of Specific taken as constants (see Term.subsumes). Neither is bound.
 */
internal object Subsumes : AbstractNativePrimitive(Signature("subsumes", 2)) {
    override fun call(call: NativeCall): Int = call.reply(call.termArg(0).subsumes(call.termArg(1)))
}
