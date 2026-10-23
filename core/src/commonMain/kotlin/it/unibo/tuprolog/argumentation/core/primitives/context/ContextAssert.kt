package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_assert(+Clause): Clause added to the active branch */
internal class ContextAssert(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_assert", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        branches.selected.assertA(request.arguments[0])
        return sequenceOf(request.replyWith(true))
    }
}
