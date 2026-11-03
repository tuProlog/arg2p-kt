package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.listArg
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.solve.Signature

/**
 * containsAny(+Elements, +List): an element of List matches one of Elements (without binding them)
 */
internal object ContainsAny : AbstractNativePrimitive(Signature("containsAny", 2)) {
    override fun call(call: NativeCall): Int {
        val elements = call.listArg(0, signature)
        val list = call.listArg(1, signature)
        return call.reply(list.any { item -> elements.any { element -> call.unifiable(item, element) } })
    }
}
