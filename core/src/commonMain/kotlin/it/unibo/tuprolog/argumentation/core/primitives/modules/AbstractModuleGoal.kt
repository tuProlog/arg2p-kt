package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.argumentation.core.runtime.modules.Module
import it.unibo.tuprolog.argumentation.core.runtime.modules.solveForCaller
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * A module call: its goal solved against the libraries of the module, loaded by [loader] at the first call.
 * The arg2p engine inlines it (DelegatedCall): the goal is solved in place, against the libraries of the module,
 * which do not see the theory of the user. Other solvers solve it in a solver of the module, cached.
 *
 * The module `user` is the theory of the user: `user::Goal` solves Goal against the theory and the libraries of the
 * resolution (those of its root), as a goal of the user would be: the only way the modules reach the theory.
 */
internal abstract class AbstractModuleGoal(
    private val loader: DynamicLoader,
) : PrimitiveWithSignature,
    DelegatedCall {
    /** What the call does before its goal is solved, in [module] */
    protected abstract fun enter(
        request: Solve.Request<ExecutionContext>,
        module: Module,
    )

    // The module and the goal of the call, checked
    private fun call(request: Solve.Request<ExecutionContext>): Pair<Atom, Struct> {
        val name = request.arguments[0]
        val goal = request.arguments[1]
        if (name !is Atom) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.ATOM, name)
        }
        if (goal !is Struct) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.CALLABLE, goal)
        }
        return name to goal
    }

    override fun delegate(request: Solve.Request<ExecutionContext>): Delegation {
        val (name, goal) = call(request)
        if (name.value == USER) return Inlining(goal, userLibraries(request.context), theory = true)
        val module = loader.modules.of(request, name)
        enter(request, module)
        return Inlining(goal, module.libraries)
    }

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (name, goal) = call(request)
        val solver =
            if (name.value == USER) {
                request.context.createMutableSolver(libraries = userLibraries(request.context))
            } else {
                val module = loader.modules.of(request, name)
                enter(request, module)
                loader.solverOf(request, module)
            }
        return sequence {
            yieldAll(
                solver.solveForCaller(goal, SolveOptions.allLazilyWithTimeout(TimeDuration.MAX_VALUE)).map {
                    when (it) {
                        // Halt carries a failed substitution, so replying with it would turn an
                        // error inside the module into an ordinary failure: indistinguishable
                        // from a goal that simply does not hold.
                        is Solution.Halt -> throw it.exception
                        else -> request.replyWith(it.substitution)
                    }
                },
            )
        }
    }

    // The libraries of the goals of the user: those of the root of the resolution (the libraries of a module call
    // only apply to its goal)
    private fun userLibraries(context: ExecutionContext): Runtime =
        (context as? PrologExecutionContext)?.pathToRoot?.last()?.libraries ?: context.libraries

    private companion object {
        /** The module of the theory of the user */
        const val USER = "user"
    }
}
