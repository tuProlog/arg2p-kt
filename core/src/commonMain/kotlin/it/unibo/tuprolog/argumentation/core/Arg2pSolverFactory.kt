package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import kotlin.js.ExperimentalJsExport
import kotlin.js.JsExport

@OptIn(ExperimentalJsExport::class)
@Suppress("NON_EXPORTABLE_TYPE")
@JsExport
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
