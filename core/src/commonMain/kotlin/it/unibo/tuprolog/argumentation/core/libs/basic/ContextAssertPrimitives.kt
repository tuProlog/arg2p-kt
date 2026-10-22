package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

// The primitives of the context changing the clauses of its active branch

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

/** context_retract(+Clause): the clauses of the active branch unifying with Clause removed */
internal class ContextRetract(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_retract", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        branches.selected.retractAll(request.arguments[0])
        return sequenceOf(request.replyWith(true))
    }
}
