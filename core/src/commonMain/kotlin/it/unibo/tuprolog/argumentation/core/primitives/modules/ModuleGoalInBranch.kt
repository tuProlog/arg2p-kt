package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.argumentation.core.runtime.modules.Module
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature

/** Module:::Goal, Goal solved against the libraries of Module, in a new branch of the active context branch */
internal class ModuleGoalInBranch(
    loader: DynamicLoader,
) : AbstractModuleGoal(loader) {
    override val signature = Signature(":::", 2)

    // A new branch of the active one, through the context library if it is reachable, otherwise solving the goals
    // of the context in a solver of the module
    override fun enter(
        module: Module,
        call: NativeCall,
    ) {
        val library = module.context
        if (library != null) {
            library.branch(library.active)
            return
        }
        enter(module) { call.context }
    }

    override fun enter(
        module: Module,
        context: () -> ExecutionContext,
    ) {
        val library = module.context
        if (library != null) {
            library.branch(library.active)
            return
        }
        val solver = context().createSolver(libraries = module.libraries)
        logicProgramming {
            solver
                .solve("context_active"(X))
                .filter { it.isYes }
                .forEach {
                    solver.solve("context_branch"(it.substitution[X]!!, `_`)).first()
                }
        }
    }
}
