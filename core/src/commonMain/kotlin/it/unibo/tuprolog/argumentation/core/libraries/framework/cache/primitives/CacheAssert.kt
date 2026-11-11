package it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.Cache
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.store.ClauseStore
import it.unibo.tuprolog.solve.Signature

/** cacheAssert(+Clause): Clause added to the cache */
internal class CacheAssert(
    private val cache: Cache,
    private val store: ClauseStore,
) : AbstractNativePrimitive(Signature("cacheAssert", 1)) {
    override fun call(call: NativeCall): Int {
        cache.follow(call)
        store.assertA(call.record(call.arg(0)))
        return NativeCall.CONTINUE
    }
}
