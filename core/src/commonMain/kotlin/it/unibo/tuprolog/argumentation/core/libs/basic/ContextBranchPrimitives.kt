package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

// The primitives of the context on its branches (see ContextBranches)

/** context_reset: back to a single empty branch */
internal class ContextReset(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_reset", 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        branches.reset()
        return sequenceOf(request.replyWith(true))
    }
}

/** context_active(?Branch): the active branch */
internal class ContextActive(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_active", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyUnifying(0, Integer.of(branches.active)))
}

/** context_checkout(+Branch): Branch made active; a branch that does not exist (never created, or dropped) is not */
internal class ContextCheckout(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_checkout", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(if (branches.checkout(request.branchArgument(0))) request.replyWith(true) else request.replyFail())
}

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

/** context_drop(+Branch): frees a branch no longer needed, fails on the active one */
internal class ContextDrop(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_drop", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(if (branches.drop(request.branchArgument(0))) request.replyWith(true) else request.replyFail())
}

/** context_last(-Branch): the most recently created branch */
internal class ContextLast(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_last", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyUnifying(0, Integer.of(branches.last)))
}

/** context_drop_after(+Branch, +Keep): drops the branches created after Branch, except those in Keep and the active one */
internal class ContextDropAfter(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_drop_after", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        branches.dropAfter(request.branchArgument(0), request.branchesArgument(1))
        return sequenceOf(request.replyWith(true))
    }
}
