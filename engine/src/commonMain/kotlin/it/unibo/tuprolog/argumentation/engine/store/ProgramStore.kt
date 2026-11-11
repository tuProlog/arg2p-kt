package it.unibo.tuprolog.argumentation.engine.store

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.vm.program.Program

/** A library of stores, whose records are in the words of the program of the solver running it */
@InternalArg2pApi
interface ProgramStore {
    /** The stores on the words of [program] */
    fun bind(program: Program)
}
