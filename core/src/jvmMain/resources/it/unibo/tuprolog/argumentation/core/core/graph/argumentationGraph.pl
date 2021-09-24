% Arguments: [Rules, TopRule, Conclusion, [LastDefRules, DefRules, DefPremises]]
% Support: (Support, Argument)
% Attack: (Attacker, Attacked, On)

buildArgumentationGraph(Rules, [Arguments, [], Supports]) :-
	    buildArguments(Rules, Arguments, Supports).
%        buildAttacks(Arguments, Supports, Attacks), !.

buildArguments(Rules, AllArguments, Supports) :-
	buildArgumentsFromPremises(Rules, Arguments),
	buildArgumentsFromRules(Rules, Arguments, AllArguments, Supports).

buildArgumentsFromPremises(Rules, Arguments) :-
    findall(
        [[PremiseID], none, Premise, [[], [], DefPrem]],
        (
            member(premise([PremiseID, Premise]), Rules),
            checkStrict(Rules, PremiseID, DefPrem)
        ),
        Arguments
    ).

% Check \+ member(RuleID, SupportRules) constraint. Is it avoiding cyclical arguments?
% Find best Cut placement

buildArgumentsFromRules(Rules, Arguments, Supports, AllArguments, AllSupports) :-
	member(rule([RuleID, RuleBody, RuleHead]), Rules),
	ruleBodyIsSupported(Arguments, RuleBody, [], [], SupportRules, ArgSupports),
	\+ member(RuleID, SupportRules),
	sort([RuleID|SupportRules], SortedPremises),
	buildArgumentInfo(Rules, ArgSupports, RuleID, Info),
    NewArgument = [SortedPremises, RuleID, RuleHead, Info],
	\+ member(NewArgument, Arguments),
	mapSupports(NewArgument, ArgSupports, MappedSupports),
    append(Supports, MappedSupports, NewSupports),
    buildArgumentsFromRules(Rules, [NewArgument|Arguments], NewSupports, AllArguments, AllSupports), !.
buildArgumentsFromRules(_, Arguments, Supports, Arguments, Supports).

mapSupports(Argument, Supports, MappedSupports) :-
    findall((S, Argument), member(S, Supports), MappedSupports).

checkStrict(Rules, Id, [Id]) :- \+ member(strict(Id), Rules).
checkStrict(Rules, Id, []) :- member(strict(Id), Rules).

% Argument Info

buildArgumentInfo(Rules, Supports, RuleId, [LastDefRules, DefRules, DefPrem]) :-
    defeasibleRules(Rules, RuleId, Supports, DefRules),
    ordinaryPremises(Supports, DefPrem),
    lastDefeasibleRules(Rules, Supports, RuleId, LastDefRules).

% Defeasible Premises

ordinaryPremises(Supports, DefPrem) :-
    findall(Def, member([_, _, _, [_, _, Def]], Supports), Prem),
    appendLists(Prem, TempPrem),
    sortDistinct(TempPrem, DefPrem).

% Defeasible rules

defeasibleRules(Rules, RuleId, Supports, DefRules) :-
	findall(Def, member([_, _, _, [_, Def, _]], Supports), UnsortedRules),
	checkStrict(Rules, RuleId, DefRule),
	appendLists([DefRule|UnsortedRules], TempRules),
	sortDistinct(TempRules, DefRules).

% Last Defeasible Rules

lastDefeasibleRules(Rules, _, TopRule, [TopRule]) :-
    TopRule \== none, \+ member(strict(TopRule), Rules).
lastDefeasibleRules(Rules, Supports, TopRule, LastRules) :-
	member(strict(TopRule), Rules),
	findall(Def, member([_, _, _, [Def, _, _]], Supports), Res),
	appendLists(Res, TempLastRules),
	sortDistinct(TempLastRules, LastRules).


ruleBodyIsSupported(_, [], ResultPremises, ResultSupports, ResultPremises, ResultSupports).
ruleBodyIsSupported(Args, [ [unless, _] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	ruleBodyIsSupported(Args, Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([ [prolog(Check)] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(Args, Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported(Args, [Statement|Others], Premises, Supports, ResultPremises, ResultSupports) :-
    member([ArgumentID, RuleID, Statement, Info], Args),
	append(ArgumentID, Premises, NewPremises),
	ruleBodyIsSupported(Others, NewPremises, [[ArgumentID, RuleID, Statement, Info]|Supports], ResultPremises, ResultSupports).


buildAttacks :-
	buildDirectAttacks,
	buildTransitiveAttacks.

buildDirectAttacks :-
	argument(A),
	argument(B),
	A \== B,
    once(attacks(T, A, B)),
	\+ attack(T, A, B, B),
	asserta(attack(T, A, B, B)),
	asserta(attack(T, A, B)),
	fail.
buildDirectAttacks.

buildTransitiveAttacks :-
	attack(T, A, B, D),
	support(B, C),
	\+ attack(T, A, C, D),
	asserta(attack(T, A, C, D)),
	asserta(attack(T, A, C)),
    buildTransitiveAttacks.
buildTransitiveAttacks.

% Attack definition
attacks(rebut, A, B) :- rebuts(A, B).
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B).
attacks(undermine, A, B) :- undermines(A, B).
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B).
attacks(undercut, A, B) :- undercuts(A, B).

strictArgument(Argument) :- argumentInfo(Argument, [_, [], []]).

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) :-
	RuleB \== none,
    \+ strictArgument([IDPremisesB, RuleB, RuleHeadB]),
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Rebutting definition: clash of a conclusion with a failure as premise assumption
%------------------------------------------------------------------------
contraryRebuts([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) :-
	RuleA \== none,
	RuleB \== none,
	rule([RuleB, Body, _]),
	recoverUnifiers(Body, [IDPremisesB, RuleB, RuleHeadB]),
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines([IDPremisesA, RuleA, RuleHeadA], [[IDPremiseB], none, RuleHeadB]) :-
	\+ strictArgument([IDPremisesB, RuleB, RuleHeadB]),
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines([IDPremisesA, none, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) :-
	RuleB \== none,
	rule([RuleB, Body, _]),
	recoverUnifiers(Body, [IDPremisesB, RuleB, RuleHeadB]),
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([_, _, [undercut(RuleB)]], [_, RuleB, _]) :-
	\+ strict(RuleB).

%------------------------------------------------------------------------
% Given a not instantiated rule and an argument, grounds the rule body using the argument support
%------------------------------------------------------------------------
recoverUnifiers(Body, Argument) :-
	findall(X, support([_, _, X], Argument), Supports),
	unifySupports(Body, Supports).

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

