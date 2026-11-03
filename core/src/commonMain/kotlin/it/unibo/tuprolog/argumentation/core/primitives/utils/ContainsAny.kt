package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.solve.Signature

/**
 * containsAny(+Elements, +List): an element of List matches one of Elements (without binding them)
 */
internal object ContainsAny : AbstractNativePrimitive(Signature("containsAny", 2)) {
    override fun call(call: NativeCall): Int {
        val elements = call.termsArg(0, signature)
        val list = call.termsArg(1, signature)
        return call.reply(list.any { item -> elements.any { element -> Arg2pUnificator.match(item, element) } })
    }
}
