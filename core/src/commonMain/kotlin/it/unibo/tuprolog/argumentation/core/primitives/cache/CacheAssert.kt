package it.unibo.tuprolog.argumentation.core.primitives.cache

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.solve.Signature

/** cache_assert(+Clause): Clause added to the cache */
internal class CacheAssert(
    private val store: ClauseStore,
) : AbstractNativePrimitive(Signature("cache_assert", 1)) {
    override fun call(call: NativeCall): Int {
        store.assertA(call.record(call.arg(0)))
        return NativeCall.CONTINUE
    }
}
