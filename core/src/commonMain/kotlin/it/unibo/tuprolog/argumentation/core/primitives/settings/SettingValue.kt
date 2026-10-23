package it.unibo.tuprolog.argumentation.core.primitives.settings

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** setting(+Name, ?Value): Value is a value of the flag Name, in the order they are stated */
internal class SettingValue(
    private val settings: Settings,
) : PrimitiveWithSignature {
    override val signature = Signature("setting", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val values = settings.valuesOf(settings.declared(request, switch = false), request.context)
        return values.asSequence().mapNotNull { value ->
            Arg2pUnificator
                .mgu(request.arguments[1], value)
                .takeIf { it.isSuccess }
                ?.let { request.replyWith(it) }
        }
    }
}
