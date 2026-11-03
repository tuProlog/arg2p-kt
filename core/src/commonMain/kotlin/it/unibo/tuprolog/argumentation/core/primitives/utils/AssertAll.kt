package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.runtime.context.contextAssertAll
import it.unibo.tuprolog.solve.Signature

/**
 * assertAll(+Clauses): each of Clauses asserted in the context
 */
internal object AssertAll : AbstractNativePrimitive(Signature("assertAll", 1)) {
    override fun call(call: NativeCall): Int {
        call.contextAssertAll(call.termsArg(0, signature))
        return NativeCall.CONTINUE
    }
}
