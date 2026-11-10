package it.unibo.tuprolog.argumentation.core.libraries.framework.cache

import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheAssert
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheCheck
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheCount
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheRetract
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers.OwnedLibrary
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
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

    // The theory what the cache holds was read from, for a solver with no versions of its theory (see follow)
    private var theory: Any? = null

    /**
     * The cache used by [call]: on a solver with no versions of its theory, what it holds is forgotten when the theory is
     * another one (a theory loaded with no context_reset after it, see refresh)
     */
    internal fun follow(call: NativeCall) {
        if (call.theoryVersion == null) follow(call.context.staticKb)
    }

    /** As follow(call), for a call of a solver with no versions of its theory, solving against [staticKb] */
    internal fun follow(staticKb: Any) {
        if (staticKb !== theory) {
            store.clear()
            theory = staticKb
        }
    }

    override val alias = "prolog.argumentation.cache"

    override val baseContent: Library
        get() =
            listOf(CacheAssert(this, store), CacheRetract(this, store), CacheCheck(this, store), CacheCount(this)).let { primitives ->
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
