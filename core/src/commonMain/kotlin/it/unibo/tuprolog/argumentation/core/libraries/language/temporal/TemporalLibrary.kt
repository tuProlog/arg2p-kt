package it.unibo.tuprolog.argumentation.core.libraries.language.temporal

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.temporal.flags.Temporal
import it.unibo.tuprolog.argumentation.core.libraries.language.temporal.prolog.THEORY

/**
 * The axioms of time (module `temporal`, with the flag temporal): the event calculus of Kowalski and Sergot (1986) as
 * defeasible rules whose conclusions hold for the times of a period, constrained (see the module constraints). A theory
 * states what happens and what it starts or ends, `happens(E, T)`, `initiates(E, U)`,
 * `terminates(E, U)`; the axioms conclude `holdsAt(U, T)`:
 * - for every T after a start at T1 (`{ T > T1 }`, the period after the event, open while no end follows);
 * - for every T up to an end at T2 the theory states (`{ T =< T2 }`, the period before the event, open while no start
 *   precedes it);
 * - for every T from a start to a later end with no end in between (`{ T > T1, T =< T2 }`, one period).
 * Any time (`holdsAt(U, 5)`, `holdsAt(U, 6.5)`) is answered by the period holding it, in query mode as on the whole
 * graph. Starting V ends what V conflicts with (the conflict model: -U for U, the declared conflicts, ...), and a start
 * while an earlier one still holds is no new start. A statement of the theory on U at a time (`-holdsAt(U, 7)`) attacks
 * the periods containing it there alone: the graph extension constraints, which the axioms require, splits each into
 * the piece at that time and those around it.
 */
object TemporalLibrary : PrologLibrary("temporal") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(Temporal)

    override val extensionFlag: ArgsFlag
        get() = Temporal

    override val prologRawTheory: String = THEORY
}
