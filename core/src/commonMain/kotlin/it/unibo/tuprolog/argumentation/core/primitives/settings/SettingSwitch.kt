package it.unibo.tuprolog.argumentation.core.primitives.settings

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.solve.Signature

/** setting(+Name): the switch Name is on */
internal class SettingSwitch(
    /** The settings this primitive checks, for the native primitives of the other libraries (see [Settings.of]) */
    val owner: Settings,
) : AbstractNativePrimitive(SIGNATURE) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        val name = call.arg(0)
        if (call.kind(name) == NativeCall.ATOM) {
            val flag = owner.flags[call.atomName(name)]
            if (flag != null && flag.isSwitch) return call.reply(owner.isOn(flag, context))
        }
        return call.reply(owner.isOn(owner.declared(call.termArg(0), context, signature, switch = true), context))
    }

    companion object {
        val SIGNATURE = Signature("setting", 1)
    }
}
