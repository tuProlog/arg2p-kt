package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.words.Symbols
import it.unibo.tuprolog.argumentation.engine.words.locked
import it.unibo.tuprolog.solve.library.Library

/**
 * The base program of the solvers: the clauses of each module (see [SharedLibrary]) are compiled once into it, when a
 * solver first asks for them, and every solver starts from a copy of it, compiling the rest alone. It grows with the
 * modules there are, by their source, whatever the solvers, their flags and their theories: it keeps no solver.
 */
@InternalArg2pApi
object SharedPrograms {
    private val base = Program()

    /** A program for a solver, of [session] if any (from the base of the session) */
    fun derived(session: SolverSession? = null): Program = locked(this) { Program(session?.base ?: base).also { it.session = session } }

    /** The base program of a new session: the symbols of the base and its code as they are now, its symbols shared */
    fun sessionBase(): Program = locked(this) { Program(base, Symbols.shared(base.symbols)) }

    /** The words of code of the base program */
    val codeSize: Int get() = locked(this) { base.codeTop }
}

/**
 * The key of the code of a library: its text is long, and a string is hashed anew at each lookup on JS. [of] keeps one for
 * each alias, for the text of a module to be hashed once in a process
 */
@InternalArg2pApi
class CodeKey(
    private val text: String,
) {
    private val hash = text.hashCode()

    override fun hashCode(): Int = hash

    override fun equals(other: Any?): Boolean = other === this || (other is CodeKey && other.hash == hash && other.text == text)

    companion object {
        private val known = HashMap<String, Pair<String, CodeKey>>()

        fun of(
            alias: String,
            theory: String,
        ): CodeKey =
            locked(known) {
                known[alias]?.takeIf { it.first == theory }?.second
                    ?: CodeKey(alias + "\n" + theory).also { known[alias] = theory to it }
            }
    }
}

/**
 * The library of a module whose clauses are the same for every instance of it, [codeKey] telling them apart: its code
 * is compiled once per process (see [SharedPrograms]), rather than for each solver
 */
@InternalArg2pApi
class SharedLibrary(
    private val library: Library,
    val codeKey: CodeKey,
) : Library by library {
    // (the hash of a library goes through all it holds, and each runtime made of it asks for it)
    private val hash by lazy { library.hashCode() }

    override fun hashCode(): Int = hash
}
