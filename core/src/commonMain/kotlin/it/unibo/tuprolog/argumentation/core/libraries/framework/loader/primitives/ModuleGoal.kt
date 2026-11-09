package it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.DynamicLoader
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers.LoadedLibrary
import it.unibo.tuprolog.solve.Signature

/** Module::Goal, Goal solved against the libraries of Module, in the active context branch */
internal class ModuleGoal(
    loader: DynamicLoader,
) : AbstractModuleGoal(loader) {
    override val signature = Signature("::", 2)

    override fun enter(module: LoadedLibrary) = Unit
}
