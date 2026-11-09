package it.unibo.tuprolog.argumentation.core.loading

import it.unibo.tuprolog.solve.Signature

/** Module::Goal, Goal solved against the libraries of Module, in the active context branch */
internal class ModuleGoal(
    loader: DynamicLoader,
) : AbstractModuleGoal(loader) {
    override val signature = Signature("::", 2)

    override fun enter(module: Module) = Unit
}
