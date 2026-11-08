package it.unibo.tuprolog.argumentation.core.primitives.graph.query

import it.unibo.tuprolog.argumentation.core.runtime.withVariablesFixed
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.argumentation.engine.vm.termArg
import it.unibo.tuprolog.solve.Signature

/**
 * groundVars(+Term, ?Grounded): Term with each of its variables replaced by an atom of its own (`$var_0`, ...):
 * a term (with none of these atoms) unifies with Grounded if and only if it generalises Term
 */
internal object GroundVars : AbstractNativePrimitive(Signature("groundVars", 2)) {
    override fun call(call: NativeCall): Int = call.reply(1, call.termArg(0).withVariablesFixed())
}
