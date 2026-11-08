package it.unibo.tuprolog.argumentation.core.runtime.modules

/** A module, loaded by its calls, which name it by its [identifier] */
interface Loadable : ArgLibrary {
    fun identifier(): String
}
