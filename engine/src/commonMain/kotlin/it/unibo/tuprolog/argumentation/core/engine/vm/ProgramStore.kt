package it.unibo.tuprolog.argumentation.core.engine.vm

/** A primitive of a store of a library, whose records are in the words of the program of the solver running it */
internal interface ProgramStore {
    /** The store on the words of [program] */
    fun bind(program: Program)
}
