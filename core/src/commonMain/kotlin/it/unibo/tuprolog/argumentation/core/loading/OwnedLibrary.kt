package it.unibo.tuprolog.argumentation.core.loading

import it.unibo.tuprolog.argumentation.engine.vm.Program
import it.unibo.tuprolog.argumentation.engine.vm.ProgramStore
import it.unibo.tuprolog.solve.library.Library

/**
 * The library of [owner], one of those of every solver (its context, its cache, its settings): the native primitives of the
 * other libraries reach the owner through it, and the machine binds the stores of the owner to its program.
 */
internal class OwnedLibrary(
    val owner: Any,
    private val library: Library,
) : Library by library,
    ProgramStore {
    override fun bind(program: Program) {
        (owner as? ProgramStore)?.bind(program)
    }

    override fun equals(other: Any?): Boolean = other is OwnedLibrary && other.library == library

    override fun hashCode(): Int = library.hashCode()
}
