package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.intArg
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_check(+Branch, Goal): the solutions of Goal in the given branch, as context_check/1 */
internal class ContextCheckIn(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_check", 2)) {
    // A branch that does not exist has no solutions
    override fun call(call: NativeCall): Int {
        val branch = call.intArg(0, signature).toInt()
        val store = branches.storeOf(branch) ?: return NativeCall.FAIL
        return call.lookupOrView(store, branchLookup(branch), 1, branch)
    }

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val store = branches.storeOf(request.branchArgument(0)) ?: return emptySequence()
        return if (isView(request.arguments[1], store)) {
            super.solve(request)
        } else {
            store.solve(request.arguments[1]).map { request.replyWith(it) }
        }
    }
}
