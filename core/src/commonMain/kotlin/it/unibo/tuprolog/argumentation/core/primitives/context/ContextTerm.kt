package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_term(+Id, ?Term): Term is the term with identifier Id (see context_id/2), its variables fresh */
internal class ContextTerm(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_term", 2)) {
    override fun call(call: NativeCall): Int {
        val id = call.arg(0)
        if (call.kind(id) != NativeCall.INT) return NativeCall.FAIL
        val record = branches.record(call.intValue(id).toInt()) ?: return NativeCall.FAIL
        return call.replyWord(1, call.recall(record))
    }
}
