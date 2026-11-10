package it.unibo.tuprolog.argumentation.core.libraries.language.conflict.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

// Conflicts derived as statements of the theory (conflict/2,3 arguments), rather than built in
object MetaConflicts : ArgsFlag {
    override fun predicate(): String = "metaConflicts"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false
}
