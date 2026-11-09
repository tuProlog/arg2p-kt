package it.unibo.tuprolog.argumentation.core.loading

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * The modules of an arg2p solver (its dynamic libraries), loaded on their first call: each solves its goals against
 * the libraries of the solver, but for the other modules, and its own.
 */
internal class Modules(
    private val solver: Arg2pSolver,
) {
    private val loaded = HashMap<String, Module>()

    private val moduleAliases: Set<String> by lazy { solver.dynamicLibraries().map { it.alias }.toSet() }

    /**
     * The module named [name], called by [request] (whose libraries the ones of the module are built from): an
     * existence error when the solver has no such module
     */
    fun of(
        request: Solve.Request<ExecutionContext>,
        name: Atom,
    ): Module = loaded.getOrPut(name.value) { load(request.context, name) }

    /** The module named [name] if it is loaded already */
    fun loaded(name: String): Module? = loaded[name]

    /** The module named [name], [context] asked for only if it is to be loaded */
    fun of(
        name: String,
        context: () -> ExecutionContext,
    ): Module = loaded[name] ?: load(context(), Atom.of(name)).also { loaded[name] = it }

    private var sharedOf: Runtime? = null
    private var sharedRuntime: Runtime? = null

    private fun load(
        context: ExecutionContext,
        name: Atom,
    ): Module {
        val library =
            solver.dynamicLibraries().firstOrNull { it.identifier() == name.value }
                ?: throw ExistenceError.forResource(context, name.value)
        // (the libraries of the solver, for all its modules)
        val callers = context.libraries
        val shared =
            sharedRuntime.takeIf { sharedOf === callers }
                ?: Runtime.of(callers.libraries.filterNot { it.alias in moduleAliases }).also {
                    sharedOf = callers
                    sharedRuntime = it
                }
        return Module(name.value, shared + library.baseContent)
    }
}
