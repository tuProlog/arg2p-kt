package it.unibo.tuprolog.argumentation.core.modules.extra

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.ModulesPath
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.argumentation.core.primitives.extra.CallModule
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Goals solved on the theories of external modules (module `module`): Prolog files, found in the modules path (flag
 * `modulesPath`) by their name, or at a location. A sandboxed solver has [sandboxed] instead.
 */
object ModuleCallsModule : ModuleCalls(CallModule) {
    /**
     * The module of a sandboxed solver (see ModuleRegistry.sandboxed): its call_module/2 reads no file nor URL, it
     * raises a permission error
     */
    val sandboxed: ArgLibrary = ModuleCalls(CallModule.Refused)
}

/** The module `module`, its call_module/2 being [callModule] */
open class ModuleCalls internal constructor(
    private val callModule: Primitive,
) : ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.modularity"

    override val baseContent: Library by lazy {
        Library.of(
            alias = this.alias,
            primitives =
                mapOf(
                    CallModule.signature to callModule,
                ),
        )
    }
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(ModulesPath)

    override fun identifier(): String = "module"
}
