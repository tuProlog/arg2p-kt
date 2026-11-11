package it.unibo.tuprolog.argumentation.core.libraries.language.moduleCalls

import it.unibo.tuprolog.argumentation.EarlierSpellings
import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.Loadable
import it.unibo.tuprolog.argumentation.core.libraries.language.moduleCalls.flags.ModulesPath
import it.unibo.tuprolog.argumentation.core.libraries.language.moduleCalls.primitives.CallModule
import it.unibo.tuprolog.solve.library.Library

/**
 * Goals solved on the theories of external modules (module `module`): Prolog files, found in the modules path (flag
 * `modulesPath`) by their name, or at a location.
 */
object ModuleCallsLibrary : Loadable {
    override val alias = "prolog.argumentation.modularity"

    override val baseContent: Library by lazy {
        Library.of(
            alias = this.alias,
            primitives = EarlierSpellings.withEarlierNames(mapOf(CallModule.signature to CallModule)),
        )
    }
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(ModulesPath)

    override fun identifier(): String = "module"
}
