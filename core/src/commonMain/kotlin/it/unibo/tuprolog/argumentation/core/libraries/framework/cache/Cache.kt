package it.unibo.tuprolog.argumentation.core.libraries.framework.cache

import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheAssert
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheCheck
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheCount
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheRetract
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers.OwnedLibrary
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.Program
import it.unibo.tuprolog.argumentation.engine.vm.ProgramStore
import it.unibo.tuprolog.solve.library.Library

/** Facts shared across context branches (cache_assert/1, cache_retract/1, cache_check/1), kept in the same indexed store as the context */
class Cache :
    ArgLibrary,
    ProgramStore {
    private val store = ClauseStore()

    // The version of the theory what the cache holds was read from: none known, for a solver with no versions
    private var version: Long? = null

    /**
     * A solve starts: what is remembered of the theory (see utils::oncePerTheory/2) is forgotten if it changed, or if
     * there is no knowing it did not (a solver with no versions of its theory)
     */
    fun refresh(theoryVersion: Long?) {
        if (theoryVersion == null || theoryVersion != version) {
            store.clear()
            version = theoryVersion
        }
    }

    override val alias = "prolog.argumentation.cache"

    override val baseContent: Library
        get() =
            listOf(CacheAssert(store), CacheRetract(store), CacheCheck(store), CacheCount()).let { primitives ->
                OwnedLibrary(
                    this,
                    Library.of(
                        alias = this.alias,
                        primitives = primitives.associateBy { it.signature },
                    ),
                )
            }

    /** The store on the words of [program] (the machine's) */
    override fun bind(program: Program) {
        // (a new program: the flags or the knowledge bases of the solver changed)
        if (store.encoding !== program) {
            store.clear()
            version = null
        }
        store.bind(program)
    }
}
