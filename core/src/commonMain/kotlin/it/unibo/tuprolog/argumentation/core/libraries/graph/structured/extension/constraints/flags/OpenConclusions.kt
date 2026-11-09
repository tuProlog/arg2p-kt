package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

// Internal: arguments may conclude open statements, holding for sets of values (see constraints::instances/0), without the
// graph extension constraints or the axioms of temporal: for the solvers given the rules of a theory written out, as those of the
// parallel solver for a theory with temporal, its axioms among its rules
object OpenConclusions : ArgsFlag {
    override fun predicate(): String = "openConclusions"

    override val isSwitch: Boolean = true

    override val isInternal: Boolean = true

    override fun default(): Boolean = false
}
