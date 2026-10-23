package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** context_unifiable(Head, Body): Body of the stored clauses whose head unifies with Head, without binding them */
internal class ContextUnifiable(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_unifiable", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        branches.selected.unifiableBodies(request.arguments[0], request.arguments[1]).map { request.replyWith(it) }
}
