package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.argumentation.core.runtime.modules.Module
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature

/** Module::Goal, Goal solved against the libraries of Module, in the active context branch */
internal class ModuleGoal(
    loader: DynamicLoader,
) : AbstractModuleGoal(loader) {
    override val signature = Signature("::", 2)

    override fun enter(
        module: Module,
        context: () -> ExecutionContext,
    ) = Unit

    override fun enter(
        module: Module,
        call: NativeCall,
    ) = Unit
}
