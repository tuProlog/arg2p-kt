package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.LibraryRegistry
import it.unibo.tuprolog.argumentation.core.libraries.Loadable
import it.unibo.tuprolog.argumentation.core.libraries.arg2p.Arg2pInterfaceLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.cache.Cache
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.Context
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.DynamicLoader
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.Settings
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.StoreSchema
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Runtime

interface Arg2pSolver {
    /** The loader of the modules, at their first call */
    val loader: ArgLibrary

    /** The context of the solver: the facts the modules assert, in branches */
    val context: ArgLibrary
    val cache: Cache
    val settings: Settings

    /** The libraries of every arg2p solver: the module loader, the context, the cache and the settings */
    fun baseLibraries(): List<ArgLibrary> = listOf(loader, context, cache, settings)

    fun staticLibraries(): Iterable<ArgLibrary>

    /** The modules, loaded by their first call */
    fun dynamicLibraries(): Iterable<Loadable>

    fun to2pLibraries() = Runtime.of(baseLibraries().plus(staticLibraries()).map { it.baseContent })

    fun operators() =
        baseLibraries()
            .plus(staticLibraries())
            .map { it.theoryOperators }
            .reduce(OperatorSet::plus)

    companion object {
        fun of(
            staticLibs: Iterable<ArgLibrary>,
            dynamicLibs: Iterable<Loadable>,
        ) = object : Arg2pSolver {
            override val loader: ArgLibrary = DynamicLoader(this)
            override val cache = Cache()
            override val context: ArgLibrary = Context(cache::refresh, StoreSchema)
            override val settings = Settings(this)

            override fun staticLibraries() = staticLibs

            override fun dynamicLibraries() = dynamicLibs
        }

        fun default(
            staticLibs: Iterable<ArgLibrary> = emptyList(),
            dynamicLibs: Iterable<Loadable> = emptyList(),
        ) = of(
            listOf(Arg2pInterfaceLibrary) + staticLibs,
            LibraryRegistry.all + dynamicLibs,
        )
    }
}
