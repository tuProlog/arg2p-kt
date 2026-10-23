@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.js

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.js.JsExport

/**
 * An arg2p solver for JS, as it is described: its theory, its flags (the default ones, unless they are given), the
 * modules it has besides those of arg2p (as the causality solver), and where what it prints goes.
 */
@JsExport
class JsSolverBuilder {
    private var theory = ""
    private var flags: String? = null
    private val modules = mutableListOf<ArgLibrary>()
    private var output: ((String) -> Unit)? = null

    /** The theory, as Prolog text */
    fun theory(text: String): JsSolverBuilder = apply { theory = text }

    /** The flags, as Prolog facts: the solver has only these, rather than the default ones */
    fun flags(text: String): JsSolverBuilder = apply { flags = text }

    /** A module the solver has besides those of arg2p, loaded by its first call */
    fun module(module: ArgLibrary): JsSolverBuilder = apply { modules += module }

    /** The function receiving what the solver prints (the standard output otherwise) */
    fun output(consumer: (String) -> Unit): JsSolverBuilder = apply { output = consumer }

    fun build(): MutableSolver {
        val stated = flags
        val settings = if (stated == null) listOf(FlagsBuilder().create()) else emptyList()
        val arg2p = Arg2pSolver.default(settings, modules.toList())
        val flagsLibrary =
            stated?.let { Runtime.of(Library.of(alias = FLAGS, clauses = Theory.parse(it, arg2p.operators()))) }
        return Arg2pSolverFactory.solverOf(
            arg2p,
            theory,
            flagsLibrary ?: Runtime.empty(),
            output?.let { OutputChannel.of(it) } ?: OutputChannel.stdOut(),
        )
    }

    private companion object {
        const val FLAGS = "prolog.argumentation.js.flags"
    }
}
