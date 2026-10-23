package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_branch(+From, ?Branch): a new branch of From, made active, only made if its identifier unifies with Branch */
internal class ContextBranch(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_branch", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val from = request.branchArgument(0)
        val unifier = Arg2pUnificator.mgu(request.arguments[1], Integer.of(branches.nextBranch))
        if (branches.storeOf(from) == null || !unifier.isSuccess) return sequenceOf(request.replyFail())
        branches.branch(from)
        return sequenceOf(request.replyWith(unifier))
    }
}
