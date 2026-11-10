package it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.Cache
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** cache_assert(+Clause): Clause added to the cache */
internal class CacheAssert(
    private val cache: Cache,
    private val store: ClauseStore,
) : AbstractNativePrimitive(Signature("cache_assert", 1)) {
    override fun call(call: NativeCall): Int {
        cache.follow(call)
        store.assertA(call.record(call.arg(0)))
        return NativeCall.CONTINUE
    }
}
