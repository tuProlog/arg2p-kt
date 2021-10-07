% Arguments: [Rules, TopRule, Conclusion, [LastDefRules, DefRules, DefPremises]]
% Support: (Support, Argument)
% Attack: (Type, Attacker, Attacked, On)

buildArgumentationGraph :-
    buildArguments,
    buildAttacks.

buildArguments :-
	buildArgumentsFromPremises,
	findall([RuleID, RuleBody, RuleHead], cache_check(rule([RuleID, RuleBody, RuleHead])), Rules),
	buildArgumentsFromRules(Rules, Rules, n).

buildArgumentsFromPremises :-
    findall(
        _,
        (
            cache_check(premise([PremiseID, Premise])),
            checkStrict(PremiseID, DefPrem),
            ground(Premise),
            cache_assert(argument([[PremiseID], none, Premise, [], [[], [], DefPrem]]))
        ),
        _
    ).

% Check \+ member(RuleID, SupportRules) constraint. Is it avoiding cyclical arguments?
% Find best Cut placement

buildArgumentsFromRules(Rules, [], n).
buildArgumentsFromRules(Rules, [], y) :- buildArgumentsFromRules(Rules, Rules, n).
buildArgumentsFromRules(Rules, [H|T], _) :-
    buildArgumentsFromRule(H),
    buildArgumentsFromRules(Rules, T, y), !.
buildArgumentsFromRules(Rules, [_|T], X) :-
    buildArgumentsFromRules(Rules, T, X).

buildArgumentsFromRule([RuleID, RuleBody, RuleHead]) :-
	ruleBodyIsSupported(RuleBody, [], [], SupportRules, ArgSupports),
	\+ member(RuleID, SupportRules),
	ground(RuleHead),
	utils::sort([RuleID|SupportRules], SortedPremises),
	\+ cache_check(argument([SortedPremises, RuleID, RuleHead, RuleBody, _])),
	buildArgumentInfo(ArgSupports, RuleID, Info),
    NewArgument = [SortedPremises, RuleID, RuleHead, RuleBody, Info],
	cache_assert(argument(NewArgument)),
	supports(NewArgument, ArgSupports).

checkStrict(Id, [Id]) :- \+ cache_check(strict(Id)).
checkStrict(Id, []) :- cache_check(strict(Id)).

% Argument Info

buildArgumentInfo(Supports, RuleId, [LastDefRules, DefRules, DefPrem]) :-
    defeasibleRules(RuleId, Supports, DefRules),
    ordinaryPremises(Supports, DefPrem),
    lastDefeasibleRules(Supports, RuleId, LastDefRules).

% Defeasible Premises

ordinaryPremises(Supports, DefPrem) :-
    findall(Def, member([_, _, _, [_, _, Def]], Supports), Prem),
    utils::appendLists(Prem, TempPrem),
    utils::sortDistinct(TempPrem, DefPrem).

% Defeasible rules

defeasibleRules(RuleId, Supports, DefRules) :-
	findall(Def, member([_, _, _, [_, Def, _]], Supports), UnsortedRules),
	checkStrict(RuleId, DefRule),
	utils::appendLists([DefRule|UnsortedRules], TempRules),
	utils::sortDistinct(TempRules, DefRules).

% Last Defeasible Rules

lastDefeasibleRules(_, TopRule, [TopRule]) :-
    TopRule \== none, \+ cache_check(strict(TopRule)).
lastDefeasibleRules(Supports, TopRule, LastRules) :-
	cache_check(strict(TopRule)),
	findall(Def, member([_, _, _, [Def, _, _]], Supports), Res),
	utils::appendLists(Res, TempLastRules),
	utils::sortDistinct(TempLastRules, LastRules).

% Argument Support

ruleBodyIsSupported([], ResultPremises, ResultSupports, ResultPremises, ResultSupports).
ruleBodyIsSupported([ [unless, _] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([ [prolog(Check)] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([Statement|Others], Premises, Supports, ResultPremises, ResultSupports) :-
    cache_check(argument([ArgumentID, RuleID, Statement, Body, Info])),
	append(ArgumentID, Premises, NewPremises),
	ruleBodyIsSupported(Others, NewPremises, [[ArgumentID, RuleID, Statement, Body, Info]|Supports], ResultPremises, ResultSupports).

supports(Argument, Supports) :-
    findall(_, (member(S, Supports), cache_assert(support(S, Argument))), _).

% Attacks

buildAttacks :-
    findall(X, cache_check(argument(X)), Args),
	buildDirectAttacks(Args),
	buildTransitiveAttacks.

buildDirectAttacks([]).
buildDirectAttacks([H|T]) :-
    findall(_, buildDirectAttack(H),_),
    buildDirectAttacks(T).

buildDirectAttack(A) :-
	cache_check(argument(B)),
	A \== B,
    attacks(T, A, B),
	\+ cache_check(attack(T, A, B, B)),
	cache_assert(attack(T, A, B, B)).

buildTransitiveAttacks :-
	cache_check(attack(T, A, B, D)),
	cache_check(support(B, C)),
	\+ cache_check(attack(T, A, C, D)), !,
	cache_assert(attack(T, A, C, D)),
    buildTransitiveAttacks.
buildTransitiveAttacks.

% Attack definition
attacks(rebut, A, B) :- rebuts(A, B), !.
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B), !.
attacks(undermine, A, B) :- undermines(A, B), !.
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B), !.
attacks(undercut, A, B) :- undercuts(A, B), !.

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts([IDPremisesA, RuleA, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, _, Info]) :-
	RuleB \== none,
    Info \== [[], [], []],
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Rebutting definition: clash of a conclusion with a failure as premise assumption
%------------------------------------------------------------------------
contraryRebuts([IDPremisesA, RuleA, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleA \== none,
	RuleB \== none,
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines([IDPremisesA, RuleA, RuleHeadA, _, _], [[IDPremiseB], none, RuleHeadB, _, Info]) :-
	Info \== [[], [], []],
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines([IDPremisesA, none, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleB \== none,
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([_, _, [undercut(RuleB)], _, _], [_, RuleB, _, _, [[RuleB], _, _]]).

%------------------------------------------------------------------------
% Given a not instantiated rule and an argument, grounds the rule body using the argument support
%------------------------------------------------------------------------
recoverUnifiers(Body, Argument) :-
	findall(X, cache_check(support([_, _, X, _], Argument)), ArgSupports),
	unifySupports(Body, ArgSupports).

unifySupports(Body, []).
unifySupports(Body, [X|T]) :- member(X, Body), unifySupports(Body, T).

%========================================================================
% CONFLICT DEFINITION
%========================================================================

conflict( [Atom], [neg, Atom]).
conflict( [neg, Atom], [Atom]).

conflict( [obl, [Atom]],  [obl, [neg, Atom]]).
conflict( [obl, [neg, Atom]],  [obl, [Atom]]).

conflict( [obl, Lit],  [neg, obl, Lit]).
conflict( [neg, obl, Lit],  [obl, Lit]).

conflict( [perm, [Atom]],  [obl, [neg, Atom]]).
conflict( [obl, [neg, Atom]],  [perm, [Atom]]).

conflict( [perm, [neg, Atom]],  [obl, [Atom]]).
conflict( [obl, [Atom]],  [perm, [neg, Atom]]).
