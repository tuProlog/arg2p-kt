package it.unibo.tuprolog.argumentation.core.loading

import it.unibo.tuprolog.argumentation.core.context.Context
import it.unibo.tuprolog.solve.library.Runtime

/** A module of an arg2p solver: the libraries its goals are solved against, those of the solver and its own */
internal class Module(
    val name: String,
    val libraries: Runtime,
) {
    /** The context library among its libraries, if any: the primitives of a runtime are a map merged when asked for */
    val context: Context? by lazy { Context.of(libraries) }
}
