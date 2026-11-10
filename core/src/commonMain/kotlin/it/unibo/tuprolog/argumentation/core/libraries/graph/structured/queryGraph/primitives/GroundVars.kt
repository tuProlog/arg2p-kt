package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.queryGraph.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers.withVariablesFixed
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.reply
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.solve.Signature

/**
 * groundVars(+Term, ?Grounded): Term with each of its variables replaced by an atom of its own (`$var_0`, ...):
 * a term (with none of these atoms) unifies with Grounded if and only if it generalises Term
 */
internal object GroundVars : AbstractNativePrimitive(Signature("groundVars", 2)) {
    override fun call(call: NativeCall): Int = call.reply(1, call.termArg(0).withVariablesFixed())
}
