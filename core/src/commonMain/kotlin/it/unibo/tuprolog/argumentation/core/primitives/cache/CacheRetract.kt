package it.unibo.tuprolog.argumentation.core.primitives.cache

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** cache_retract(+Clause): the clauses of the cache unifying with Clause removed */
internal class CacheRetract(
    private val store: ClauseStore,
) : PrimitiveWithSignature {
    override val signature = Signature("cache_retract", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        store.retractAll(request.arguments[0].castToStruct())
        return sequenceOf(request.replyWith(true))
    }
}
