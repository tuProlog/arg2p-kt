package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.branchLookup
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.isView
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.lookup
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.lookupOrView
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * contextCheck(Goal): the solutions of Goal in the active branch (see [it.unibo.tuprolog.argumentation.engine.store.ClauseStore.solve]); the machine
 * resolves Goal with the records of the branch itself (see [lookup])
 */
internal class ContextCheck(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("contextCheck", 1)) {
    override fun call(call: NativeCall): Int = call.lookupOrView(branches.selected, branchLookup(branches.active), 0, null, branches.layout)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        if (isView(request.arguments[0], branches.selected, branches.layout)) {
            super.solve(request)
        } else {
            branches.selected.solve(request.arguments[0]).map { request.replyWith(it) }
        }
}
