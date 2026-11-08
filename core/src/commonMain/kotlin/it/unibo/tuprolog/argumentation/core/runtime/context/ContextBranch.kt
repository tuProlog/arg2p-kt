package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.intArg
import it.unibo.tuprolog.solve.Signature

/** context_branch(+From, ?Branch): a new branch of From, made active, only made if its identifier unifies with Branch */
internal class ContextBranch(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_branch", 2)) {
    override fun call(call: NativeCall): Int {
        val from = call.intArg(0, signature).toInt()
        if (branches.storeOf(from) == null || !call.unify(call.arg(1), call.int(branches.nextBranch))) return NativeCall.FAIL
        branches.branch(from)
        return NativeCall.CONTINUE
    }
}
