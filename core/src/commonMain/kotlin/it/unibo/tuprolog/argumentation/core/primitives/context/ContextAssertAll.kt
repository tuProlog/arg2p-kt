package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_assert_all(+Terms): context_assert/1 of every term in the list, in order */
internal class ContextAssertAll(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_assert_all", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        request.arguments[0]
            .castToList()
            .toList()
            .forEach { branches.selected.assertA(it) }
        return sequenceOf(request.replyWith(true))
    }
}
