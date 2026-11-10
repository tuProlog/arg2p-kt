package it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.attempts
import it.unibo.tuprolog.solve.Signature

/**
 * extension_flag(?Library, ?Flag): the library Library, among those of the solver, is an extension of the theory when
 * Flag is on. See [it.unibo.tuprolog.argumentation.core.libraries.Loadable.extensionFlag].
 */
internal class ExtensionFlag(
    private val solver: Arg2pSolver,
) : AbstractNativePrimitive(Signature("extension_flag", 2)) {
    // The libraries an extension by a flag, and their flags, by name: the libraries of a solver do not change
    private val flagged by lazy {
        solver.dynamicLibraries().mapNotNull { library -> library.extensionFlag?.let { library.identifier() to it.predicate() } }
    }

    override fun call(call: NativeCall): Int {
        val matching = flagged.filter { (library, flag) -> call.fits(call.arg(0), library) && call.fits(call.arg(1), flag) }
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
