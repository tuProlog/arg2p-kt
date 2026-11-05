package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.MachineCall
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_id(+Term, ?Id): the integer identifier of Term (see [ContextBranches.identifier]) */
internal class ContextId(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_id", 2)) {
    override fun call(call: NativeCall): Int {
        val term = call.arg(0)
        // (most terms have one by now: a copy of them only for those that do not)
        val known =
            (call as? MachineCall)?.let { machine ->
                machine.recordKey(term)?.let { branches.knownIdentifier(it, machine.recordProgram) }
            }
        return call.replyWord(1, call.int(known ?: branches.identifier(call.record(term))))
    }
}
