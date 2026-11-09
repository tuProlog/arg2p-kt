%! superiorArgument(+A, +B)
%  A is superior to B.
superiorArgument(A, B) :-
    store::argumentInfo(A, InfoA),
    store::argumentInfo(B, InfoB),
    superiorInfo(InfoA, InfoB, _).

%! superiorArgument(+A, +B, -Preferences)
%  A is superior to B, by the preferences between rules Preferences (sup/2 terms, a set): each justification of
%  the superiority in turn, [] if it needs no preference.
superiorArgument(A, B, Preferences) :-
    store::argumentInfo(A, InfoA),
    store::argumentInfo(B, InfoB),
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

%! superiorById(+InfoIdA, +InfoIdB)
%  The arguments with the info of identifier InfoIdA (see store::infoById/2) are superior to those with the info of
%  identifier InfoIdB: compared once for each pair of identifiers, as the info is shared by many arguments
%  (infoSuperior/3 in the context).
superiorById(InfoIdA, InfoIdB) :-
    context_check(infoSuperior(InfoIdA, InfoIdB, Result)), !,
    Result == yes.
superiorById(InfoIdA, InfoIdB) :-
    store::infoById(InfoIdA, InfoA),
    store::infoById(InfoIdB, InfoB),
    compareInfo(InfoA, InfoB, _), !,
    context_assert(infoSuperior(InfoIdA, InfoIdB, yes)).
superiorById(InfoIdA, InfoIdB) :-
    context_assert(infoSuperior(InfoIdA, InfoIdB, no)),
    fail.

%! compareInfo(+InfoA, +InfoB, -Preferences)
%  The arguments with info InfoA are superior to those with info InfoB, by the preferences Preferences, each in turn.
compareInfo(InfoA, InfoB, Preferences) :-
    store::infoParts(InfoA, LastDefRulesA, DefRulesA, DefPremisesA),
    store::infoParts(InfoB, LastDefRulesB, DefRulesB, DefPremisesB),
    superiorArgumentSupport(LastDefRulesA, DefRulesA, DefPremisesA, LastDefRulesB, DefRulesB, DefPremisesB, Preferences).

% The normal comparator (Dung and Thang) is defined on the last rules of the arguments: last link only
superiorArgumentSupport(_, _, _, _, _, _, _) :-
    setting(orderingComparator, normal),
    setting(orderingPrinciple, Principle),
    Principle \== last, !,
    throw(error(domain_error(orderingPrinciple(last), Principle), normal)).
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
    preference(X, W).

weakerDemo([], _, []).
weakerDemo([H|T], Rules, [Sup|Preferences]) :-
    singleStronger(H, Rules, Sup),
    weakerDemo(T, Rules, Preferences).

singleStronger(Target, Rules, sup(Rule, Target)) :-
    member(Rule, Rules),
    preference(Rule, Target).

allStronger(_, [], []).
allStronger(Target, [Rule|Rules], [sup(Rule, Target)|Preferences]) :-
    preference(Rule, Target),
    allStronger(Target, Rules, Preferences).
