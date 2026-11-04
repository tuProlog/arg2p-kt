package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_id(+Term, ?Id): the integer identifier of Term (see [ContextBranches.identifier]) */
internal class ContextId(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_id", 2)) {
    override fun call(call: NativeCall): Int = call.replyWord(1, call.int(branches.identifier(call.record(call.arg(0))).toLong()))
}
