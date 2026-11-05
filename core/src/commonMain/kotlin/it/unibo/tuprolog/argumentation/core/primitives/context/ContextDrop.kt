package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.engine.reply
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_drop(+Branch): frees a branch no longer needed, fails on the active one */
internal class ContextDrop(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_drop", 1)) {
    override fun call(call: NativeCall): Int = call.reply(branches.drop(call.intArg(0, signature).toInt()))
}
