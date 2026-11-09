package it.unibo.tuprolog.argumentation.core.cache

import it.unibo.tuprolog.argumentation.core.loading.OwnedLibrary
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.Program
import it.unibo.tuprolog.argumentation.engine.vm.ProgramStore
import it.unibo.tuprolog.solve.library.Library

/** Facts shared across context branches (cache_assert/1, cache_retract/1, cache_check/1), kept in the same indexed store as the context */
class Cache :
    ArgLibrary,
    ProgramStore {
    private val store = ClauseStore()

    override val alias = "prolog.argumentation.cache"

    override val baseContent: Library
        get() =
            listOf(CacheAssert(store), CacheRetract(store), CacheCheck(store)).let { primitives ->
                OwnedLibrary(
                    this,
                    Library.of(
                        alias = this.alias,
                        primitives = primitives.associateBy { it.signature },
                    ),
                )
            }

    /** The store on the words of [program] (the machine's) */
    override fun bind(program: Program) = store.bind(program)
}
