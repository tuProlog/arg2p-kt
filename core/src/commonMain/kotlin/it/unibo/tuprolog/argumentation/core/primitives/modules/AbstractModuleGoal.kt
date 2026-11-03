package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.NativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.argumentation.core.runtime.modules.Module
import it.unibo.tuprolog.argumentation.core.runtime.modules.haltsRaised
import it.unibo.tuprolog.argumentation.core.runtime.modules.solveForCaller
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.currentTimeInstant
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
    DelegatedCall,
    NativePrimitive {
    /** What the call does before its goal is solved, in [module]; [request] made only if it is needed */
    protected abstract fun enter(
        module: Module,
        request: () -> Solve.Request<ExecutionContext>,
    )

    // The machine: the module's libraries entered on the goal as a word, no term built
    override fun call(call: NativeCall): Int {
        val name = call.arg(0)
        val goal = call.arg(1)
        if (call.kind(name) != NativeCall.ATOM) {
            throw TypeError.forGoal(call.context, signature, TypeError.Expected.ATOM, call.term(name))
        }
        if (!call.isCallable(goal)) {
            throw TypeError.forGoal(call.context, signature, TypeError.Expected.CALLABLE, call.term(goal))
        }
        val moduleName = call.atomName(name)
        if (moduleName == USER) return call.inline(goal, call.rootLibraries, theoryVisible = true)
        val request = { Solve.Request(signature, listOf(call.term(name), call.term(goal)), call.context) }
        val module = loader.modules.of(moduleName, request)
        enter(module, request)
        return call.inline(goal, module.libraries, theoryVisible = false)
    }

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
        enter(module) { request }
        return Inlining(goal, module.libraries)
    }

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (name, goal) = call(request)
        val solver =
            if (name.value == USER) {
                request.context.createMutableSolver(libraries = userLibraries(request.context))
            } else {
                val module = loader.modules.of(request, name)
                enter(module) { request }
                loader.solverOf(request, module)
            }
        // the time the resolution has left
        val remaining = request.context.let { it.maxDuration - (currentTimeInstant() - it.startTime) }.coerceAtLeast(0)
        return sequence {
            yieldAll(
                solver
                    .solveForCaller(
                        goal,
                        SolveOptions.allLazilyWithTimeout(remaining),
                    ).haltsRaised()
                    .map { request.replyWith(it.substitution) },
            )
        }
    }

    // The libraries of the goals of the user: those of the root of the resolution (the libraries of a module call
    // only apply to its goal)
    private fun userLibraries(context: ExecutionContext): Runtime =
        (context as? PrologExecutionContext)?.root?.libraries ?: context.libraries

    private companion object {
        /** The module of the theory of the user */
        const val USER = "user"
    }
}
