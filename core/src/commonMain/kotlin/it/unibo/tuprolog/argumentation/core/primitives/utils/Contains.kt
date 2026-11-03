package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.solve.Signature

/**
 * contains(?Element, +List): an element of List matches Element (without binding it)
 */
internal object Contains : AbstractNativePrimitive(Signature("contains", 2)) {
    override fun call(call: NativeCall): Int {
        val element = call.termArg(0)
        return call.reply(call.termsArg(1, signature).any { Arg2pUnificator.match(it, element) })
    }
}
