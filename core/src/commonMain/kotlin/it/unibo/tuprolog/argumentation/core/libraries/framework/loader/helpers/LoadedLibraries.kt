package it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers

import it.unibo.tuprolog.argumentation.EarlierSpellings
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
internal class LoadedLibraries(
    private val solver: Arg2pSolver,
) {
    private val loaded = HashMap<String, LoadedLibrary>()

    private val moduleAliases: Set<String> by lazy { solver.dynamicLibraries().map { it.alias }.toSet() }

    /**
     * The module named [name], called by [request] (whose libraries the ones of the module are built from): an
     * existence error when the solver has no such module
     */
    fun of(
        request: Solve.Request<ExecutionContext>,
        name: Atom,
    ): LoadedLibrary = loaded.getOrPut(name.value) { load(request.context, name) }

    /** The module named [name] if it is loaded already */
    fun loaded(name: String): LoadedLibrary? = loaded[name]

    /** The module named [name], [context] asked for only if it is to be loaded */
    fun of(
        name: String,
        context: () -> ExecutionContext,
    ): LoadedLibrary = loaded[name] ?: load(context(), Atom.of(name)).also { loaded[name] = it }

    // What each module defines, its predicates as name/arity, by the name of the module: a module's library does not change
    private val defined = HashMap<String, Set<String>>()

    /**
     * The predicates the module named [name] defines, as name/arity (see [Loadable.definitions]), the module not loaded
     * for it: an existence error, [context] asked for, when the solver has no such module
     */
    fun definedBy(
        name: String,
        context: () -> ExecutionContext,
    ): Set<String> =
        defined.getOrPut(name) {
            solver.dynamicLibraries().firstOrNull { it.identifier() == current(name) }?.definitions
                ?: throw ExistenceError.forResource(context(), name)
        }

    // The identifier of the module named [name], named as before 0.26.0 maybe (see EarlierSpellings)
    private fun current(name: String): String = EarlierSpellings.values[name] ?: name

    private var sharedOf: Runtime? = null
    private var sharedRuntime: Runtime? = null

    private fun load(
        context: ExecutionContext,
        name: Atom,
    ): LoadedLibrary {
        val library =
            solver.dynamicLibraries().firstOrNull { it.identifier() == current(name.value) }
                ?: throw ExistenceError.forResource(context, name.value)
        // (the libraries of the solver, for all its modules)
        val callers = context.libraries
        val shared =
            sharedRuntime.takeIf { sharedOf === callers }
                ?: Runtime.of(callers.libraries.filterNot { it.alias in moduleAliases }).also {
                    sharedOf = callers
                    sharedRuntime = it
                }
        return LoadedLibrary(name.value, shared + library.baseContent)
    }
}
