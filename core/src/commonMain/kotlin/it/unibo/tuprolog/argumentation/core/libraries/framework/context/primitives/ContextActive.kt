package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.replyWord
import it.unibo.tuprolog.solve.Signature

/** context_active(?Branch): the active branch */
internal class ContextActive(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_active", 1)) {
    override fun call(call: NativeCall): Int = call.replyWord(0, call.int(branches.active))
}
