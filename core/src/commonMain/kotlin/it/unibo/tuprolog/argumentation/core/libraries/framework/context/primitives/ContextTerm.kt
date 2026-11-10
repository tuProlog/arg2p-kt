package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
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
