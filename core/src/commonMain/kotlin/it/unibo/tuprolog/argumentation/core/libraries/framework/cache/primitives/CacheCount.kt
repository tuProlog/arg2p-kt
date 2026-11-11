package it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.Cache
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
import it.unibo.tuprolog.solve.Signature

/**
 * cacheCount(+Goal, +Limit, -Count): the number of solutions of Goal in the cache, counted up to Limit
 * (an integer, or anything else for no limit): enough to compare how selective goals are, without visiting all
 * the solutions of the least selective ones
 */
internal class CacheCount(
    private val cache: Cache,
) : AbstractNativePrimitive(Signature("cacheCount", 3)) {
    override fun call(call: NativeCall): Int {
        cache.follow(call)
        val limitWord = call.arg(1)
        val limit = if (call.kind(limitWord) == NativeCall.INT) call.intValue(limitWord).toInt() else Int.MAX_VALUE
        val count = call.countSolutions(call.struct("cacheCheck", intArrayOf(call.arg(0))), limit)
        return call.replyWord(2, call.int(count))
    }
}
