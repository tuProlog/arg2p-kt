package it.unibo.tuprolog.argumentation.core.flags

/**
 * A flag of arg2p, declared by the library it configures and checked through setting/1,2 (see
 * [it.unibo.tuprolog.argumentation.core.runtime.settings.Settings]): a theory states its values as facts of its
 * [predicate]
 */
interface ArgsFlag<T, G> {
    fun predicate(): String

    fun default(): T

    fun values(): G

    /** Whether the flag is a switch, on when stated by its predicate alone (`queryMode.`), rather than taking values */
    val isSwitch: Boolean
        get() = false
}
