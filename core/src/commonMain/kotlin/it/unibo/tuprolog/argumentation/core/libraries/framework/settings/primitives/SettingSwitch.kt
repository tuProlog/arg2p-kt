package it.unibo.tuprolog.argumentation.core.libraries.framework.settings.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.Settings
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.argumentation.engine.vm.termArg
import it.unibo.tuprolog.solve.Signature

/** setting(+Name): the switch Name is on */
internal class SettingSwitch(
    private val owner: Settings,
) : AbstractNativePrimitive(Signature("setting", 1)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        val name = call.arg(0)
        if (call.kind(name) == NativeCall.ATOM) {
            val flag = owner.flags[call.atomName(name)]
            if (flag != null && flag.isSwitch) return call.reply(owner.isOn(flag, context))
        }
        return call.reply(owner.isOn(owner.declared(call.termArg(0), context, signature, switch = true), context))
    }
}
