package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.causality.modules.CausalityModule
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import kotlin.js.JsExport

/** arg2p as a library: its solver, with every module of arg2p and of the causality solver, and its version */
@JsExport
object Arg2pLibrary {
    /**
     * The libraries of a solver of arg2p, with no flags (the theory states them, or a library of FlagsBuilder.create()
     * added): the causality predicates are loaded by `causality::`
     */
    @JsExport.Ignore
    fun get() = Arg2pSolver.default(dynamicLibs = listOf(CausalityModule()))

    /** The version of arg2p, the released one for a release */
    fun version(): String = ARG2P_VERSION
}
