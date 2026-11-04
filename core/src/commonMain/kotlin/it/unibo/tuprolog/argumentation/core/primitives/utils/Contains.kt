package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.listArg
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.solve.Signature

/**
 * contains(?Element, +List): an element of List matches Element (without binding it)
 */
internal object Contains : AbstractNativePrimitive(Signature("contains", 2)) {
    override fun call(call: NativeCall): Int {
        val element = call.arg(0)
        return call.reply(call.listArg(1, signature).any { call.unifiable(it, element) })
    }
}
