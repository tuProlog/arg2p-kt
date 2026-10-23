package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.argumentation.core.runtime.modules.Module
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/** Module:::Goal, Goal solved against the libraries of Module, in a new branch of the active context branch */
internal class ModuleGoalInBranch(
    loader: DynamicLoader,
) : AbstractModuleGoal(loader) {
    override val signature = Signature(":::", 2)

    // A new branch of the active one, through the context library if it is reachable, otherwise solving the goals
    // of the context in a solver of the module
    override fun enter(
        request: Solve.Request<ExecutionContext>,
        module: Module,
    ) {
        val context = Context.of(module.libraries)
        if (context != null) {
            context.branch(context.active)
            return
        }
        val solver = request.context.createSolver(libraries = module.libraries)
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
