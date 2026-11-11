package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.MachineCall
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
import it.unibo.tuprolog.solve.Signature

/** contextId(+Term, ?Id): the integer identifier of Term (see [ContextBranches.identifier]) */
internal class ContextId(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("contextId", 2)) {
    override fun call(call: NativeCall): Int {
        val term = call.arg(0)
        // (most terms have one by now: a copy of them only for those that do not)
        val known =
            (call as? MachineCall)?.let { machine ->
                branches.knownIdentifier(machine.recordKey(term), machine.recordProgram)
            }
        return call.replyWord(1, call.int(known ?: branches.identifier(call.record(term))))
    }
}
