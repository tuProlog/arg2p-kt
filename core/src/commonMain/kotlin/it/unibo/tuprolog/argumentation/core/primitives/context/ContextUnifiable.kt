package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.attempts
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_unifiable(Head, Body): Body of the stored clauses whose head unifies with Head, without binding them */
internal class ContextUnifiable(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_unifiable", 2)) {
    override fun call(call: NativeCall): Int {
        val head = call.arg(0)
        if (call.kind(head) == NativeCall.VAR) return NativeCall.FAIL
        val matching =
            branches.selected.candidates(call.record(head)).filter { stored ->
                val w = call.recall(stored.record)
                call.unifiable(head, call.headOf(stored.record, w))
            }
        return call.attempts(
            matching
                .asSequence()
                .map { stored -> { c: NativeCall -> c.unify(c.arg(1), c.bodyOf(stored.record, c.recall(stored.record))) } }
                .iterator(),
        )
    }
}
