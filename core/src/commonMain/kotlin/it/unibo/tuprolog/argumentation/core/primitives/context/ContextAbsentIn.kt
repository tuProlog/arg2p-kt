package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.argumentation.core.runtime.context.branchArgument
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_absent(+Branch, Goal): `\+ context_check(Branch, Goal)` as a single step; a branch that does not exist has none */
internal class ContextAbsentIn(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_absent", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val store = branches.storeOf(request.branchArgument(0))
        return sequenceOf(request.replyWith(store?.solve(request.arguments[1])?.none() ?: true))
    }
}
