package it.unibo.tuprolog.argumentation.core.runtime.cache

import it.unibo.tuprolog.argumentation.core.runtime.context.lookup
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.GoalWrapper
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * cache_check(Goal): the solutions of Goal in the cache (see [ClauseStore.solve]); the machine resolves Goal with
 * the records of the cache itself (see [lookup])
 */
internal class CacheCheck(
    private val store: ClauseStore,
) : AbstractNativePrimitive(Signature("cache_check", 1)) {
    override fun call(call: NativeCall): Int = call.lookup(store, LOOKUP, 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        store.solve(request.arguments[0].castToStruct()).map { request.replyWith(it) }

    private companion object {
        // The lookup of a goal in the cache: cache_check(Goal)
        val LOOKUP = GoalWrapper { call, goal -> call.struct("cache_check", intArrayOf(goal)) }
    }
}
