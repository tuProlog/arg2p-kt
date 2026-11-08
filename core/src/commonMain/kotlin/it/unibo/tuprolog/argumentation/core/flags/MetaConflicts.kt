package it.unibo.tuprolog.argumentation.core.flags

// Conflicts derived as statements of the theory (conflict/2,3 arguments), rather than built in
object MetaConflicts : ArgsFlag {
    override fun predicate(): String = "metaConflicts"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false
}
