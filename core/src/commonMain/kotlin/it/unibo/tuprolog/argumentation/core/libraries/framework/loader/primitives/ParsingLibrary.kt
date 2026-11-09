package it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.attempts
import it.unibo.tuprolog.solve.Signature

/**
 * parsing_library(?Library, ?Flag): the library Library, among those of the solver, reads part of a theory when Flag is
 * on. See [it.unibo.tuprolog.argumentation.core.libraries.Loadable.parsingFlag].
 */
internal class ParsingLibrary(
    private val solver: Arg2pSolver,
) : AbstractNativePrimitive(Signature("parsing_library", 2)) {
    // The libraries reading part of a theory, and their flags, by name: the libraries of a solver do not change
    private val parsing by lazy {
        solver.dynamicLibraries().mapNotNull { library -> library.parsingFlag?.let { library.identifier() to it.predicate() } }
    }

    override fun call(call: NativeCall): Int {
        val matching = parsing.filter { (library, flag) -> call.fits(call.arg(0), library) && call.fits(call.arg(1), flag) }
        return call.attempts(
            matching
                .map { (library, flag) ->
                    { it: NativeCall -> it.unify(it.arg(0), it.atom(library)) && it.unify(it.arg(1), it.atom(flag)) }
                }.iterator(),
        )
    }

    // Whether the word [w] may be the atom [name]
    private fun NativeCall.fits(
        w: Int,
        name: String,
    ): Boolean =
        when (kind(w)) {
            NativeCall.VAR -> true
            NativeCall.ATOM -> atomName(w) == name
            else -> false
        }
}
