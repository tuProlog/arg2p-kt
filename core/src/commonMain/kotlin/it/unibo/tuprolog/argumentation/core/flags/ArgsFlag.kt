package it.unibo.tuprolog.argumentation.core.flags

/**
 * A flag of arg2p, declared by the library it configures and checked through setting/1,2 (see
 * [it.unibo.tuprolog.argumentation.core.settings.Settings]): a theory states its values as facts of its
 * [predicate]
 */
interface ArgsFlag {
    fun predicate(): String

    /** The default of the flag: for a switch whether it is on, otherwise its value or values */
    fun default(): Any

    /** The values the flag takes, those it declares (see [isOpen]): none for a switch */
    fun values(): Iterable<String> = emptyList()

    /** Whether the flag is a switch, on when stated by its predicate alone (`queryMode.`), rather than taking values */
    val isSwitch: Boolean
        get() = false

    /** Whether the flag is for the solvers of arg2p alone, not for its users: not among the flags a solver tells */
    val isInternal: Boolean
        get() = false

    /**
     * Whether the flag takes values beyond those it declares ([values]): the names of modules, which a user may add
     * (graph extensions, say). A value a closed flag does not declare is an error.
     */
    val isOpen: Boolean
        get() = false

    /**
     * The values standing for several others, by value: a theory stating one of them states the values it stands
     * for, and the settings tell those (as for the builtin of conflictModel, all its parts)
     */
    val aliases: Map<String, List<String>>
        get() = emptyMap()
}
