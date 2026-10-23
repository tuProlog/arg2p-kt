package it.unibo.tuprolog.argumentation.core.modules

/** A module, loaded by its calls, which name it by its [identifier] */
interface Loadable {
    fun identifier(): String
}
