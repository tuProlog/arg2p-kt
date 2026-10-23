package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.argumentation.core.runtime.context.branchesArgument
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

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
