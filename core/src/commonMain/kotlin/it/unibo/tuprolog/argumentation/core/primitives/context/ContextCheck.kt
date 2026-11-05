package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * context_check(Goal): the solutions of Goal in the active branch (see [it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore.solve]); the machine
 * resolves Goal with the records of the branch itself (see [lookup])
 */
internal class ContextCheck(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_check", 1)) {
    override fun call(call: NativeCall): Int = call.lookupOrView(branches.selected, branchLookup(branches.active), 0, null)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        if (isView(request.arguments[0], branches.selected)) {
            super.solve(request)
        } else {
            branches.selected.solve(request.arguments[0]).map { request.replyWith(it) }
        }
}
