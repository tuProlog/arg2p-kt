package it.unibo.tuprolog.argumentation.core.primitives.cache

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.primitives.context.retractAll
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.solve.Signature

/** cache_retract(+Clause): the clauses of the cache unifying with Clause removed */
internal class CacheRetract(
    private val store: ClauseStore,
) : AbstractNativePrimitive(Signature("cache_retract", 1)) {
    override fun call(call: NativeCall): Int {
        call.retractAll(store, call.arg(0))
        return NativeCall.CONTINUE
    }
}
