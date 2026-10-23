package it.unibo.tuprolog.argumentation.core.primitives.settings

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** setting(+Name): the switch Name is on */
internal class SettingSwitch(
    /** The settings this primitive checks, for the native primitives of the other libraries (see [Settings.of]) */
    val owner: Settings,
) : PrimitiveWithSignature {
    override val signature = SIGNATURE

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyWith(owner.isOn(owner.declared(request, switch = true), request.context)))

    companion object {
        val SIGNATURE = Signature("setting", 1)
    }
}
