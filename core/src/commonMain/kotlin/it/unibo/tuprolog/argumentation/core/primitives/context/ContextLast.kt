package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_last(-Branch): the most recently created branch */
internal class ContextLast(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_last", 1)) {
    override fun call(call: NativeCall): Int = call.replyWord(0, call.int(branches.last))
}
