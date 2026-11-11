package it.unibo.tuprolog.argumentation.core.libraries.framework.cache

import it.unibo.tuprolog.argumentation.EarlierSpellings
import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheAssert
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheCheck
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheCount
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.primitives.CacheRetract
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers.OwnedLibrary
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.store.ClauseStore
import it.unibo.tuprolog.argumentation.engine.store.ProgramStore
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.solve.library.Library

/** Facts shared across context branches (cacheAssert/1, cacheRetract/1, cacheCheck/1), kept in the same indexed store as the context */
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

    // The theory what the cache holds was read from, for a solver with no versions of its theory (see follow), and the
    // copies of it the solvers of the modules were given (see copied)
    private var theories: List<Any> = emptyList()

    /**
     * The cache used by [call]: on a solver with no versions of its theory, what it holds is forgotten when the theory is
     * another one (a theory loaded with no contextReset after it, see refresh)
     */
    internal fun follow(call: NativeCall) {
        if (call.theoryVersion == null) follow(call.context.staticKb)
    }

    /** As follow(call), for a call of a solver with no versions of its theory, solving against [staticKb] */
    internal fun follow(staticKb: Any) {
        if (theories.none { it === staticKb }) {
            store.clear()
            theories = listOf(staticKb)
        }
    }

    /**
     * The solver of a module given [copy] of [staticKb], the theory of its caller (tuProlog's solvers load a copy of the
     * theory they are given): the calls solved against either solve against the same theory
     */
    internal fun copied(
        staticKb: Any,
        copy: Any,
    ) {
        follow(staticKb)
        if (theories.none { it === copy }) theories = theories + copy
    }

    override val alias = "prolog.argumentation.cache"

    override val baseContent: Library
        get() =
            listOf(CacheAssert(this, store), CacheRetract(this, store), CacheCheck(this, store), CacheCount(this)).let { primitives ->
                OwnedLibrary(
                    this,
                    Library.of(
                        alias = this.alias,
                        primitives = EarlierSpellings.withEarlierNames(primitives.associateBy { it.signature }),
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
