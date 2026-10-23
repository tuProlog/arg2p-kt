package it.unibo.tuprolog.argumentation.core.modules.extra

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.ModulesPath
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.argumentation.core.primitives.extra.CallModule
import it.unibo.tuprolog.solve.library.Library

object ModuleCallsModule : ArgLibrary, Loadable {
    override val alias = "prolog.argumentation.modularity"

    override val baseContent: Library by lazy {
        Library.of(
            alias = this.alias,
            primitives =
                mapOf(
                    CallModule.signature to CallModule,
                ),
        )
    }
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(ModulesPath)

    override fun identifier(): String = "module"
}
