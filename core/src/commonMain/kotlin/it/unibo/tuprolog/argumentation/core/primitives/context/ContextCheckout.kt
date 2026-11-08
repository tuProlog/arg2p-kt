package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.intArg
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.solve.Signature

/** context_checkout(+Branch): Branch made active; a branch that does not exist (never created, or dropped) is not */
internal class ContextCheckout(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_checkout", 1)) {
    override fun call(call: NativeCall): Int = call.reply(branches.checkout(call.intArg(0, signature).toInt()))
}
