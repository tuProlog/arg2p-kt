package it.unibo.tuprolog.argumentation.core.flags

// Internal: arguments may conclude open statements, holding for sets of values (see parser::instances/0), without the
// graph extension constraints or the axioms of temporal: for the solvers of a theory already converted, as those of the
// parallel solver for a theory with temporal, its axioms among its rules
object OpenConclusions : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "openConclusions"

    override val isSwitch: Boolean = true

    override val isInternal: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
