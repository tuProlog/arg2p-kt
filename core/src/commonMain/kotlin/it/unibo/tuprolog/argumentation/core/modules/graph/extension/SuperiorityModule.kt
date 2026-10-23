package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.OrderingComparator
import it.unibo.tuprolog.argumentation.core.flags.OrderingPrinciple
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * When an argument is superior to another one (module `superiority`), by the preferences between their rules
 * (`sup(Rule, Other)` in the context). Two arguments are compared on their defeasible rules and premises (their
 * info, see the module argument): the last defeasible rules or all of them (orderingPrinciple last or weakest),
 * compared as sets by orderingComparator (elitist, democrat or normal).
 */
object SuperiorityModule : PrologModule("prolog.argumentation.superiority", "superiority") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(OrderingPrinciple, OrderingComparator)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! superiorArgument(+A, +B)
        %  A is superior to B.
        superiorArgument(A, B) :-
            argument::info(A, InfoA),
            argument::info(B, InfoB),
            superiorInfo(InfoA, InfoB, _).

        %! superiorArgument(+A, +B, -Preferences)
        %  A is superior to B, by the preferences between rules Preferences (sup/2 terms, a set): each justification of
        %  the superiority in turn, [] if it needs no preference.
        superiorArgument(A, B, Preferences) :-
            argument::info(A, InfoA),
            argument::info(B, InfoB),
            justifications(InfoA, InfoB, Justifications),
            member(Preferences, Justifications).

        %! justifications(+InfoA, +InfoB, -Justifications)
        %  The sets of preferences by which the arguments with info InfoA are superior to those with info InfoB, each
        %  a set, without duplicates (none if they are not): computed once for each pair (justified/3 in the context).
        justifications(InfoA, InfoB, Justifications) :-
            context_check(justified(InfoA, InfoB, Justifications)), !.
        justifications(InfoA, InfoB, Justifications) :-
            findall(Sorted, (compareInfo(InfoA, InfoB, Preferences), utils::sortDistinct(Preferences, Sorted)), All),
            utils::sortDistinct(All, Justifications),
            context_assert(justified(InfoA, InfoB, Justifications)).

        %! superiorInfo(+InfoA, +InfoB, -Preferences)
        %  The arguments with info InfoA are superior to those with info InfoB, by the preferences Preferences.
        %  Superiority only depends on the info of the arguments, much smaller and more often shared than whole
        %  arguments: it is computed on them, once for each pair (superior/4 in the context).
        superiorInfo(InfoA, InfoB, _) :-
            context_check(superior(InfoA, InfoB, _, false)), !,
            fail.
        superiorInfo(InfoA, InfoB, Preferences) :-
            context_check(superior(InfoA, InfoB, Preferences, true)), !.
        superiorInfo(InfoA, InfoB, Preferences) :-
            compareInfo(InfoA, InfoB, Preferences), !,
            context_assert(superior(InfoA, InfoB, Preferences, true)).
        superiorInfo(InfoA, InfoB, _) :-
            context_assert(superior(InfoA, InfoB, [], false)),
            fail.

        compareInfo(InfoA, InfoB, Preferences) :-
            argument::infoParts(InfoA, LastDefRulesA, DefRulesA, DefPremisesA),
            argument::infoParts(InfoB, LastDefRulesB, DefRulesB, DefPremisesB),
            superiorArgumentSupport(LastDefRulesA, DefRulesA, DefPremisesA, LastDefRulesB, DefRulesB, DefPremisesB, Preferences).

        superiorArgumentSupport(LastDefRulesA, _, DefPremisesA, LastDefRulesB, _, DefPremisesB, Preferences) :-
            setting(orderingPrinciple, last),
            superior(LastDefRulesA, DefPremisesA, LastDefRulesB, DefPremisesB, Preferences).
        superiorArgumentSupport(_, DefRulesA, DefPremisesA, _, DefRulesB, DefPremisesB, Preferences) :-
            setting(orderingPrinciple, weakest),
            superior(DefRulesA, DefPremisesA, DefRulesB, DefPremisesB, Preferences).

        % superior(+RulesA, +PremisesA, +RulesB, +PremisesB, -Preferences): the rules and premises of A are superior
        % to those of B
        superior([], PremisesA, [], PremisesB, Preferences) :-
            weaker(PremisesB, PremisesA, Preferences).
        superior(DefRulesA, _, DefRulesB, _, Preferences) :-
            setting(orderingPrinciple, last),
            (DefRulesA \== []; DefRulesB \== []),
            weaker(DefRulesB, DefRulesA, Preferences).
        superior(DefRulesA, [], DefRulesB, [], Preferences) :-
            setting(orderingPrinciple, weakest),
            weaker(DefRulesB, DefRulesA, Preferences).
        superior(DefRulesA, PremisesA, DefRulesB, PremisesB, Preferences) :-
            setting(orderingPrinciple, weakest),
            (DefRulesA \== []; DefRulesB \== []),
            (PremisesA \== []; PremisesB \== []),
            weaker(DefRulesB, DefRulesA, PreferencesOnRules),
            weaker(PremisesB, PremisesA, PreferencesOnPremises),
            utils::appendLists([PreferencesOnRules, PreferencesOnPremises], Preferences).

        % weaker(+RulesA, +RulesB, -Preferences): the set RulesA is weaker than RulesB, by orderingComparator, each
        % justification in turn
        weaker(RulesA, [], []) :-
            RulesA \== [].
        weaker(RulesA, RulesB, Preferences) :-
            RulesA \== [],
            RulesB \== [],
            setting(orderingComparator, elitist),
            member(Rule, RulesA),
            allStronger(Rule, RulesB, Preferences).
        weaker(RulesA, RulesB, Preferences) :-
            RulesA \== [],
            RulesB \== [],
            setting(orderingComparator, democrat),
            weakerDemo(RulesA, RulesB, Preferences).
        % (A, B) in attnr(K) iff 1. A undercuts B, or 2. A rebuts B (at B') and there is no defeasible rule
        % d in ldr(A) such that d < last(B')
        weaker(RulesA, RulesB, [sup(X, W)]) :-
            RulesA \== [],
            RulesB \== [],
            setting(orderingComparator, normal),
            member(W, RulesA),
            member(X, RulesB),
            context_check(sup(X, W)).

        weakerDemo([], _, []).
        weakerDemo([H|T], Rules, [Sup|Preferences]) :-
            singleStronger(H, Rules, Sup),
            weakerDemo(T, Rules, Preferences).

        singleStronger(Target, Rules, sup(Rule, Target)) :-
            member(Rule, Rules),
            context_check(sup(Rule, Target)).

        allStronger(_, [], []).
        allStronger(Target, [Rule|Rules], [sup(Rule, Target)|Preferences]) :-
            context_check(sup(Rule, Target)),
            allStronger(Target, Rules, Preferences).
        """.trimIndent()
}
