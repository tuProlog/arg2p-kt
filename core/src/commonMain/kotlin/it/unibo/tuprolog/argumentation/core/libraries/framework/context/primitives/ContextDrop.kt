package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.intArg
import it.unibo.tuprolog.argumentation.engine.natives.reply
import it.unibo.tuprolog.solve.Signature

/** context_drop(+Branch): frees a branch no longer needed, fails on the active one */
internal class ContextDrop(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_drop", 1)) {
    override fun call(call: NativeCall): Int = call.reply(branches.drop(call.intArg(0, signature).toInt()))
}
