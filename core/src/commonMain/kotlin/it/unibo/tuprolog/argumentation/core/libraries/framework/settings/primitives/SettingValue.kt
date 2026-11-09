package it.unibo.tuprolog.argumentation.core.libraries.framework.settings.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.Settings
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.answers
import it.unibo.tuprolog.argumentation.engine.vm.attempts
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.argumentation.engine.vm.termArg
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature

/** setting(+Name, ?Value): Value is a value of the flag Name, in the order they are stated */
internal class SettingValue(
    private val settings: Settings,
) : AbstractNativePrimitive(Signature("setting", 2)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        val answer = onAtoms(call, context)
        if (answer != UNDECIDED) return answer
        val values = settings.valuesOf(settings.declared(call.termArg(0), context, signature, switch = false), context)
        return call.answers(1, values.iterator())
    }

    // The common cases with no term of the call and no unification of terms: a flag of atoms for a variable (each value in
    // turn, on backtracking), an atom to find among the values; what it does not decide goes the general way, errors
    // included
    private fun onAtoms(
        call: NativeCall,
        context: ExecutionContext,
    ): Int {
        val name = call.arg(0)
        if (call.kind(name) != NativeCall.ATOM) return UNDECIDED
        val flag = settings.flags[call.atomName(name)]?.takeIf { !it.isSwitch } ?: return UNDECIDED
        val values = settings.valuesOf(flag, context)
        val target = call.arg(1)
        return when (call.kind(target)) {
            NativeCall.VAR ->
                when {
                    values.size == 1 -> call.reply(1, values[0])
                    values.any { it !is Atom } -> UNDECIDED
                    else ->
                        call.attempts(
                            values
                                .map { value -> { it: NativeCall -> it.unify(it.arg(1), it.atom((value as Atom).value)) } }
                                .iterator(),
                        )
                }
            NativeCall.ATOM -> {
                if (values.any { it !is Atom }) return UNDECIDED
                val wanted = call.atomName(target)
                when (values.count { (it as Atom).value == wanted }) {
                    0 -> NativeCall.FAIL
                    1 -> NativeCall.CONTINUE
                    else -> UNDECIDED
                }
            }
            else -> UNDECIDED
        }
    }

    private companion object {
        const val UNDECIDED = -1
    }
}
