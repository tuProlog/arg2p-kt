package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.runtime.subsumes
import it.unibo.tuprolog.solve.Signature

/**
 * subsumes(+General, +Specific): Specific is an instance of General (subsumes_term/2, which the solver lacks): General
 * unifies with Specific, the variables of Specific taken as constants (see Term.subsumes). Neither is bound.
 */
internal object Subsumes : AbstractNativePrimitive(Signature("subsumes", 2)) {
    override fun call(call: NativeCall): Int = call.reply(call.termArg(0).subsumes(call.termArg(1)))
}
