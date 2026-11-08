package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.InternalArg2pApi

/** A primitive of a store of a library, whose records are in the words of the program of the solver running it */
@InternalArg2pApi
interface ProgramStore {
    /** The store on the words of [program] */
    fun bind(program: Program)
}
