package it.unibo.tuprolog.argumentation.core.modules.extra

import it.unibo.tuprolog.argumentation.core.flags.ModulesPath
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.primitives.extra.ModuleCall
import it.unibo.tuprolog.solve.library.Library

object ModuleCalls : ArgLibrary, Loadable {
    override val alias = "prolog.argumentation.modularity"

    override val baseContent: Library by lazy {
        Library.of(
            alias = this.alias,
            primitives =
                mapOf(
                    ModuleCall.signature to ModuleCall,
                ),
        )
    }
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(ModulesPath)

    override fun identifier(): String = "module"
}
