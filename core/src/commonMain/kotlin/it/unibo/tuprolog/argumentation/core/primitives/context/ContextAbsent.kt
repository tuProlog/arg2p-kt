package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_absent(Goal): `\+ context_check(Goal)` as a single step */
internal class ContextAbsent(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_absent", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyWith(branches.selected.solve(request.arguments[0]).none()))
}
