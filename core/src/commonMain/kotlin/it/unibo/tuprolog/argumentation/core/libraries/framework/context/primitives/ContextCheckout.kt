package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.intArg
import it.unibo.tuprolog.argumentation.engine.natives.reply
import it.unibo.tuprolog.solve.Signature

/** contextCheckout(+Branch): Branch made active; a branch that does not exist (never created, or dropped) is not */
internal class ContextCheckout(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("contextCheckout", 1)) {
    override fun call(call: NativeCall): Int = call.reply(branches.checkout(call.intArg(0, signature).toInt()))
}
