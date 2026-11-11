package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
import it.unibo.tuprolog.solve.Signature

/** contextActive(?Branch): the active branch */
internal class ContextActive(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("contextActive", 1)) {
    override fun call(call: NativeCall): Int = call.replyWord(0, call.int(branches.active))
}
