package it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.listArg
import it.unibo.tuprolog.argumentation.engine.vm.reply
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
