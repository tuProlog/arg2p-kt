package it.unibo.tuprolog.argumentation.core.runtime.modules

import it.unibo.tuprolog.solve.library.Runtime

/** A module of an arg2p solver: the libraries its goals are solved against, those of the solver and its own */
internal class Module(
    val name: String,
    val libraries: Runtime,
)
