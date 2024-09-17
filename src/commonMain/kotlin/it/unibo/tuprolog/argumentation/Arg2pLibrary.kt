package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.Arg2pSolver

object Arg2pLibrary {
    fun get() = Arg2pSolver.default()

    fun version() = "0.7.0"
}
