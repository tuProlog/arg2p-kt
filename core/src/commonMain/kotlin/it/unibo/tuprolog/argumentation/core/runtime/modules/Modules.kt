package it.unibo.tuprolog.argumentation.core.runtime.modules

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.error.DomainError
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * The modules of an arg2p solver (its dynamic libraries), loaded on their first call: each solves its goals against
 * the libraries of the solver, but for the other modules, and its own.
 */
class Modules(
    private val solver: Arg2pSolver,
) {
    private val loaded = HashMap<String, Module>()

    private val moduleAliases: Set<String> by lazy { solver.dynamicLibraries().map { it.alias }.toSet() }

    /** The module named [name], called by [request] (whose libraries the ones of the module are built from) */
    fun of(
        request: Solve.Request<ExecutionContext>,
        name: Atom,
    ): Module = loaded.getOrPut(name.value) { load(request, name) }

    /** Forgets the modules loaded so far */
    fun clear() = loaded.clear()

    private fun load(
        request: Solve.Request<ExecutionContext>,
        name: Atom,
    ): Module {
        val library =
            solver.dynamicLibraries().firstOrNull { (it as? Loadable)?.identifier() == name.value }
                ?: throw DomainError.forGoal(request.context, request.signature, DomainError.Expected.of("Loadable Lib"), name)
        val callerLibraries = request.context.libraries.libraries
        val shared = Runtime.of(callerLibraries.filterNot { it.alias in moduleAliases })
        return Module(name.value, shared + library.content())
    }
}
