package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** loader_reset, the modules loaded so far forgotten: their next call loads them again */
internal class LoaderReset(
    private val loader: DynamicLoader,
) : PrimitiveWithSignature {
    override val signature = Signature("loader_reset", 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        loader.reset()
        return sequenceOf(request.replyWith(true))
    }
}
