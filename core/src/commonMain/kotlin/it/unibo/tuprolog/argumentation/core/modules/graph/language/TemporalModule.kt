package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The axioms of time (module `temporal`, with the flag temporal): the event calculus of Kowalski and Sergot (1986) as
 * defeasible rules over regions of time (see the module constraints). A theory states what happens and what it starts
 * or ends, `happens(E, T)`, `initiates(E, U)`, `terminates(E, U)`; the axioms conclude where U holds:
 * - `holdsAt(U, from(T1))`, after a start at T1 (the period after the event, open while no end follows);
 * - `holdsAt(U, until(T2))`, up to an end at T2 the theory states (the period before the event, open while no start
 *   precedes it);
 * - `holdsAt(U, between(T1, T2))`, from a start to a later end with no end in between (one period).
 * A time asked for is in the region of one of them (`holdsAt(U, 5)`). Starting V ends what V conflicts with (the
 * conflict model: -U for U, the declared conflicts, ...), and a start while an earlier one still holds is no new start. A statement of the theory on U at a time
 * (`-holdsAt(U, 7)`) conflicts with the regions at that time alone.
 */
object TemporalModule : PrologModule("prolog.argumentation.language.temporal", "temporal") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! rules(-Rules)
        %  The axioms, as the parser reads the defeasible rules of a theory ([Id, Premises, Conclusion]).
        rules([
            % an event starts or ends U at its time; starting V ends what V conflicts with
            [t_start(E, U, T), (happens(E, T), initiates(E, U)), started(U, T)],
            [t_stop(E, U, T), (happens(E, T), terminates(E, U)), ended(U, T)],
            [t_ends(E, U, T), (happens(E, T), initiates(E, V), prolog(conflict::between([V], [U]))), ended(U, T)],
            % the periods after a start, before an end the theory states (a start of the complement tells nothing of
            % what held before it), and from a start to a later end
            [t_after(U, T1), started(U, T1), holdsAt(U, from(T1))],
            [t_before(E, U, T2), (happens(E, T2), terminates(E, U)), holdsAt(U, until(T2))],
            [t_period(U, T1, T2), (started(U, T1), ended(U, T2), prolog(T1 < T2)), holdsAt(U, between(T1, T2))],
            % an end in between: not one period
            [t_broken(U, T1, T2, T), (started(U, T1), ended(U, T2), ended(U, T), prolog((T1 < T, T < T2))), -t_period(U, T1, T2)],
            % an end after a start closes the period after it, a start before an end the period before it
            [t_closes(U, T1, T2), (started(U, T1), ended(U, T2), prolog(T1 < T2)), -t_after(U, T1)],
            [t_opens(E, U, T1, T2), (started(U, T1), happens(E, T2), terminates(E, U), prolog(T1 < T2)), -t_before(E, U, T2)],
            % a start while an earlier start still holds (no end in between) is no new start
            [t_already(E, U, T0, T), (started(U, T0), happens(E, T), initiates(E, U), prolog(T0 < T)), -t_start(E, U, T)],
            [t_still(E, U, T0, T, Te), (started(U, T0), ended(U, Te), prolog((T0 < Te, Te =< T))), -t_already(E, U, T0, T)]
        ]).
        """.trimIndent()
}
