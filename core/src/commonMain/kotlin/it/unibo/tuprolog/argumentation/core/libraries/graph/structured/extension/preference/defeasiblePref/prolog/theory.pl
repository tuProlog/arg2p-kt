%! preferenceMode
%  This graph extension weighs the attacks by the preferences (see phases::preferenceMode/1).
preferenceMode.

%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see phases::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  The attacks the preferences may invalidate, as arguments, attacked by the arguments for the preferences, each
%  on its own (see modifyArgumentationGraph/1).
modifyArgumentationGraph :-
    modifyArgumentationGraph(each).

%! modifyArgumentationGraph(+Model)
%  As modifyArgumentationGraph/0, the attack arguments attacked by an argument for each preference invalidating
%  them (Model each), or for all of them together, preference(Preferences) (Model all, the extension
%  defeasibleAllPref).
modifyArgumentationGraph(Model) :-
    assertAllSup,
    store::directAttacks(Attacks),
    filterSupRelatedAttacks(Attacks, InvalidAttacks),
    attackArguments(InvalidAttacks),
    buildPrefAttacks(Model).

%! assertAllSup
%  The preferences concluded by the arguments, as the preferences (see superiority::statePreferences/1), in place of
%  those the theory states: only the concluded ones count.
assertAllSup :-
    findall(sup(RuleOne, RuleTwo), (
        store::argumentFor([sup(RuleOne, RuleTwo)], Argument)
    ), Concluded),
    utils::deduplicate(Concluded, Preferences),
    superiority::statePreferences(Preferences).

%! filterSupRelatedAttacks(+Attacks, -Invalid)
%  Invalid are the attacks among Attacks that the preferences invalidate. Those invalid whatever the
%  preferences (C superior to A with none) are removed, as standardPref does: no argument could save them.
filterSupRelatedAttacks([], []).
filterSupRelatedAttacks([(T, A, C)|Attacks], Invalid) :-
    invalid(T, A, C, C, []), !,
    store::removeAttack(T, A, C, C),
    filterSupRelatedAttacks(Attacks, Invalid).
filterSupRelatedAttacks([(T, A, C)|Attacks], [(T, A, C)|Invalid]) :-
    invalid(T, A, C, C, _), !,
    filterSupRelatedAttacks(Attacks, Invalid).
filterSupRelatedAttacks([_|Attacks], Invalid) :-
    filterSupRelatedAttacks(Attacks, Invalid).

%! invalid(+Type, +A, +B, +C, -Preferences)
%  The attack of A on B (on C) is invalidated by the preferences Preferences, C being superior to A: each
%  justification of the superiority in turn (see superiority::superiorArgument/3).
invalid(rebut, A, _, C, Preferences) :- superiority::superiorArgument(C, A, Preferences).
invalid(undermine, A, _, C, Preferences) :- superiority::superiorArgument(C, A, Preferences).

%! attackArguments(+Attacks)
%  Each attack becomes an argument for it: an attack of A on B becomes the argument X for attack(T, A, B, B),
%  supported by A and attacking B. If A attacks B through C, a sub-argument of B, the argument for the attack
%  on C attacks B, as the attacks on C extend to the arguments built on it (see the module store).
attackArguments(List) :-
    member((T, A, B), List),
    generateDirectAttackArgument(T, A, B, B),
    fail.
attackArguments(_).

% The attacks on A extend to the argument for the attack, which A supports
generateDirectAttackArgument(T, A, B, B) :-
    store::infoParts(Info, [attack], [attack], []),
    store::newArgument([attack], attack, [attack(T, A, B, B)], [], Info, AttackArgument),
    store::saveArgument([attack(T, A, B, B)], AttackArgument),
    store::saveSupport(A, AttackArgument),
    store::saveAttack(T, AttackArgument, B, B),
    store::removeAttack(T, A, B, B).

%! attackArgument(?AttackArgument, ?Type, ?A, ?B, ?C)
%  AttackArgument is the saved argument for the attack attack(Type, A, B, C).
attackArgument(AttackArgument, Type, A, B, C) :-
    store::topRule(AttackArgument, attack),
    store::argumentFor([attack(Type, A, B, C)], AttackArgument).

% The arguments for the preferences invalidating an attack, by any of its justifications, attack its argument
% (and so what it supports): each of them (each), or one for all of them together (all), for each choice of
% an argument for each preference (see argumentsFor/2), each standing or falling with its own
buildPrefAttacks(each) :-
    attackArgument(Attack, T, A, B, C),
    invalid(T, A, B, C, Preferences),
    member(X, Preferences),
    store::argumentFor([X], Preference),
    store::saveAttack(pref, Preference, Attack, Attack),
    fail.
buildPrefAttacks(all) :-
    attackArgument(Attack, T, A, B, C),
    invalid(T, A, B, C, Preferences),
    argumentsFor(Preferences, Supports),
    createSuperiorityArgument(Preferences, Supports, Argument),
    store::saveAttack(pref, Argument, Attack, Attack),
    fail.
buildPrefAttacks(_).

% Supports is each choice of one argument for each of Preferences, in their order
argumentsFor([], []).
argumentsFor([X|Xs], [Support|Supports]) :-
    store::argumentFor([X], Support),
    argumentsFor(Xs, Supports).

% The argument for preference(Preferences) supported by Supports, one argument for each of them, and so attacked
% as they are (see the module store): saved once, for the first attack the preferences invalidate
createSuperiorityArgument(Preferences, Supports, Argument) :-
    mergeIds(Supports, MergedId),
    store::infoParts(Info, [pref], MergedId, []),
    store::newArgument(MergedId, pref, [preference(Preferences)], [], Info, Argument),
    (store::saveArgument([preference(Preferences)], Argument) -> store::saveSupporters(Argument, Supports) ; true).

% The rules of the arguments, after pref
mergeIds([], [pref]).
mergeIds(Arguments, [pref|MergedId]) :-
    findall(Rules, (member(A, Arguments), store::argumentRules(A, Rules)), Res),
    utils::appendLists(Res, MergedId).

%==============================================================================
% QUERY MODE
%==============================================================================

%! queryDependencies(+Argument, -Statement)
%  The preferences between the rules of Argument are themselves conclusions (sup/2).
queryDependencies(Argument, S) :-
    store::argumentRules(Argument, Rules),
    member(Rule, Rules),
    (S = sup(Rule, _) ; S = sup(_, Rule)).

%! queryDependants(+Argument, -Statement)
%  An argument for a preference relates the arguments built on its rules.
queryDependants(Argument, S) :-
    store::claim(Argument, sup(X, Y)),
    (Rule = X ; Rule = Y),
    parser::conclusionOf(Rule, S).
