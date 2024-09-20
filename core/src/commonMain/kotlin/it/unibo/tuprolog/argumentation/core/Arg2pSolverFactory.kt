package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary

object Arg2pSolverFactory {
    fun of(
        staticLibs: Iterable<ArgLibrary>,
        dynamicLibs: Iterable<ArgLibrary>,
    ) = Arg2pSolver.of(staticLibs, dynamicLibs)

    fun default(
        staticLibs: Iterable<ArgLibrary> = emptyList(),
        dynamicLibs: Iterable<ArgLibrary> = emptyList(),
    ) = Arg2pSolver.default(
        staticLibs,
        dynamicLibs,
    )
}
