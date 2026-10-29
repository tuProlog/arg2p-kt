package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.modules.ArgContext
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.ArgLoader
import it.unibo.tuprolog.argumentation.core.modules.ModuleRegistry
import it.unibo.tuprolog.argumentation.core.modules.entry.Arg2pModule
import it.unibo.tuprolog.argumentation.core.runtime.cache.Cache
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Runtime

interface Arg2pSolver {
    val loader: ArgLoader
    val context: ArgContext
    val cache: Cache
    val settings: Settings

    /** The libraries of every arg2p solver: the module loader, the context, the cache and the settings */
    fun baseLibraries(): List<ArgLibrary> = listOf(loader, context, cache, settings)

    fun staticLibraries(): Iterable<ArgLibrary>

    fun dynamicLibraries(): Iterable<ArgLibrary>

    fun to2pLibraries() = Runtime.of(baseLibraries().plus(staticLibraries()).map { it.content() })

    fun operators() =
        baseLibraries()
            .plus(staticLibraries())
            .map { it.theoryOperators }
            .reduce(OperatorSet::plus)

    companion object {
        fun of(
            staticLibs: Iterable<ArgLibrary>,
            dynamicLibs: Iterable<ArgLibrary>,
        ) = object : Arg2pSolver {
            override val loader: ArgLoader = DynamicLoader(this)
            override val context: ArgContext = Context()
            override val cache = Cache()
            override val settings = Settings(this)

            override fun staticLibraries() = staticLibs

            override fun dynamicLibraries() = dynamicLibs
        }

        fun default(
            staticLibs: Iterable<ArgLibrary> = emptyList(),
            dynamicLibs: Iterable<ArgLibrary> = emptyList(),
        ) = of(
            listOf(Arg2pModule) + staticLibs,
            ModuleRegistry.all + dynamicLibs,
        )
    }
}
