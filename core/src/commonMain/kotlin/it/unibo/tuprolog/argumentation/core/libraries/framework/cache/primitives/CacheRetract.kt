package it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.Cache
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.retractAll
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** cache_retract(+Clause): the clauses of the cache unifying with Clause removed */
internal class CacheRetract(
    private val cache: Cache,
    private val store: ClauseStore,
) : AbstractNativePrimitive(Signature("cache_retract", 1)) {
    override fun call(call: NativeCall): Int {
        cache.follow(call)
        call.retractAll(store, call.arg(0))
        return NativeCall.CONTINUE
    }
}
