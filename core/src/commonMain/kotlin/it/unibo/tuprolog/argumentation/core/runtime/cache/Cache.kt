package it.unibo.tuprolog.argumentation.core.runtime.cache

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.primitives.cache.CacheAssert
import it.unibo.tuprolog.argumentation.core.primitives.cache.CacheCheck
import it.unibo.tuprolog.argumentation.core.primitives.cache.CacheRetract
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.solve.library.Library

/** Facts shared across context branches (cache_assert/1, cache_retract/1, cache_check/1), kept in the same indexed store as the context */
class Cache : ArgLibrary {
    private val store = ClauseStore()

    override val alias = "prolog.argumentation.cache"

    override val baseContent: Library
        get() =
            listOf(CacheAssert(store), CacheRetract(store), CacheCheck(store)).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}
