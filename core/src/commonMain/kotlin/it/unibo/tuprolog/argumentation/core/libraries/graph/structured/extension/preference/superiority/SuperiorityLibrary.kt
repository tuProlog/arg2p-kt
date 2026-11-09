package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.flags.OrderingComparator
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.flags.OrderingPrinciple
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.prolog.THEORY

/**
 * When an argument is superior to another one (module `superiority`), by the preferences between their rules
 * (`sup(Rule, Other)`: those the theory states, closed transitively, or those defeasiblePref states in their place). Two arguments are compared on their defeasible rules and premises (their
 * info, see the module argument): the last defeasible rules or all of them (orderingPrinciple last or weakest),
 * compared as sets by orderingComparator (elitist, democrat or normal).
 */
object SuperiorityLibrary : PrologLibrary("superiority") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(OrderingPrinciple, OrderingComparator)

    override val prologRawTheory: String = THEORY
}
