package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.StoreClauses
import it.unibo.tuprolog.argumentation.core.runtime.context.branchLookup
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * context_check(Goal): the solutions of Goal in the active branch (see [it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore.solve]); the arg2p engine
 * resolves Goal with the clauses of the branch itself (see [StoreClauses])
 */
internal class ContextCheck(
    private val branches: ContextBranches,
) : PrimitiveWithSignature,
    DelegatedCall {
    override val signature = Signature("context_check", 1)

    override fun delegate(request: Solve.Request<ExecutionContext>): Delegation? =
        StoreClauses.delegation(branches.selected, branchLookup(branches.active), request.arguments[0], request.context.libraries)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        branches.selected.solve(request.arguments[0]).map { request.replyWith(it) }
}
