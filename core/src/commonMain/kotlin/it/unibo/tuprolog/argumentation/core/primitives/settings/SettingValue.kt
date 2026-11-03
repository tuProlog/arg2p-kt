package it.unibo.tuprolog.argumentation.core.primitives.settings

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.answers
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.solve.Signature

/** setting(+Name, ?Value): Value is a value of the flag Name, in the order they are stated */
internal class SettingValue(
    private val settings: Settings,
) : AbstractNativePrimitive(Signature("setting", 2)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        val values = settings.valuesOf(settings.declared(call.termArg(0), context, signature, switch = false), context)
        return call.answers(1, values.iterator())
    }
}
