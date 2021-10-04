% Arguments: [Rules, TopRule, Conclusion, [LastDefRules, DefRules, DefPremises]]
% Support: (Support, Argument)
% Attack: (Type, Attacker, Attacked, On)

buildArgumentationGraph(Rules, [SortedArguments, SortedAttacks, SortedSupports]) :-
    buildArguments(Rules, Arguments, Supports),
    buildAttacks(Rules, Arguments, Supports, Attacks),
    utils::sort(Arguments, SortedArguments),
    utils::sort(Attacks, SortedAttacks),
    utils::sort(Supports, SortedSupports).

buildArguments(Rules, AllArguments, Supports) :-
	buildArgumentsFromPremises(Rules, Arguments),
	buildArgumentsFromRules(Rules, Arguments, [], AllArguments, Supports).

buildArgumentsFromPremises(Rules, Arguments) :-
    findall(
        [[PremiseID], none, Premise, [[], [], DefPrem]],
        (
            member(premise([PremiseID, Premise]), Rules),
            checkStrict(Rules, PremiseID, DefPrem),
            ground(Premise)
        ),
        Arguments
    ).

% Check \+ member(RuleID, SupportRules) constraint. Is it avoiding cyclical arguments?
% Find best Cut placement

buildArgumentsFromRules(Rules, Arguments, Supports, AllArguments, AllSupports) :-
	member(rule([TempRuleID, TempRuleBody, TempRuleHead]), Rules),
	copy_term([TempRuleID, TempRuleBody, TempRuleHead], [RuleID, RuleBody, RuleHead]),
	ruleBodyIsSupported(Arguments, RuleBody, [], [], SupportRules, ArgSupports),
	\+ member(RuleID, SupportRules),
	ground(RuleHead),
	utils::sort([RuleID|SupportRules], SortedPremises),
	buildArgumentInfo(Rules, ArgSupports, RuleID, Info),
    NewArgument = [SortedPremises, RuleID, RuleHead, Info],
	\+ member(NewArgument, Arguments), !,
	mapSupports(NewArgument, ArgSupports, MappedSupports),
    append(Supports, MappedSupports, NewSupports),
    buildArgumentsFromRules(Rules, [NewArgument|Arguments], NewSupports, AllArguments, AllSupports).
buildArgumentsFromRules(_, Arguments, Supports, Arguments, Supports).

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
    utils::appendLists(Prem, TempPrem),
    utils::sortDistinct(TempPrem, DefPrem).

% Defeasible rules

defeasibleRules(Rules, RuleId, Supports, DefRules) :-
	findall(Def, member([_, _, _, [_, Def, _]], Supports), UnsortedRules),
	checkStrict(Rules, RuleId, DefRule),
	utils::appendLists([DefRule|UnsortedRules], TempRules),
	utils::sortDistinct(TempRules, DefRules).

% Last Defeasible Rules

lastDefeasibleRules(Rules, _, TopRule, [TopRule]) :-
    TopRule \== none, \+ member(strict(TopRule), Rules).
lastDefeasibleRules(Rules, Supports, TopRule, LastRules) :-
	member(strict(TopRule), Rules),
	findall(Def, member([_, _, _, [Def, _, _]], Supports), Res),
	utils::appendLists(Res, TempLastRules),
	utils::sortDistinct(TempLastRules, LastRules).

% Argument Support

ruleBodyIsSupported(_, [], ResultPremises, ResultSupports, ResultPremises, ResultSupports).
ruleBodyIsSupported(Args, [ [unless, _] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	ruleBodyIsSupported(Args, Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported(Args, [ [prolog(Check)] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(Args, Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported(Args, [Statement|Others], Premises, Supports, ResultPremises, ResultSupports) :-
    member([ArgumentID, RuleID, Statement, Info], Args),
	append(ArgumentID, Premises, NewPremises),
	ruleBodyIsSupported(Args, Others, NewPremises, [[ArgumentID, RuleID, Statement, Info]|Supports], ResultPremises, ResultSupports).

mapSupports(Argument, Supports, MappedSupports) :-
    findall((S, Argument), member(S, Supports), MappedSupports).

% Attacks

buildAttacks(Rules, Arguments, Supports, Attacks) :-
	buildDirectAttacks(Rules, Arguments, Supports, [], DirAttacks),
	buildTransitiveAttacks(Rules, Arguments, Supports, DirAttacks, Attacks).

buildDirectAttacks(Rules, Arguments, Supports, Attacks, ResAttacks) :-
	member(A, Arguments),
	member(B, Arguments),
	A \== B,
    attacks(Rules, Supports, T, A, B),
	\+ member((T, A, B, B), Attacks), !,
	buildDirectAttacks(Rules, Arguments, Supports, [(T, A, B, B)|Attacks], ResAttacks).
buildDirectAttacks(_, _, _, Attacks, Attacks).

buildTransitiveAttacks(Rules, Arguments, Supports, Attacks, ResAttacks) :-
	member((T, A, B, D), Attacks),
	member((B, C), Supports),
	\+ member((T, A, C, D), Attacks), !,
    buildTransitiveAttacks(Rules, Arguments, Supports, [(T, A, C, D)|Attacks], ResAttacks).
buildTransitiveAttacks(_, _, _, Attacks, Attacks).

% Attack definition
attacks(Rules, Supports, rebut, A, B) :- rebuts(Rules, Supports, A, B), !.
attacks(Rules, Supports, contrary_rebut, A, B) :- contraryRebuts(Rules, Supports, A, B), !.
attacks(Rules, Supports, undermine, A, B) :- undermines(Rules, Supports, A, B), !.
attacks(Rules, Supports, contrary_undermine, A, B) :- contraryUndermines(Rules, Supports, A, B), !.
attacks(Rules, Supports, undercut, A, B) :- undercuts(Rules, Supports, A, B), !.

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts(_, _, [IDPremisesA, RuleA, RuleHeadA, _], [IDPremisesB, RuleB, RuleHeadB, Info]) :-
	RuleB \== none,
    Info \== [[], [], []],
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Rebutting definition: clash of a conclusion with a failure as premise assumption
%------------------------------------------------------------------------
contraryRebuts(Rules, Supports, [IDPremisesA, RuleA, RuleHeadA, _], [IDPremisesB, RuleB, RuleHeadB, Info]) :-
	RuleA \== none,
	RuleB \== none,
	member(rule([RuleB, Body, _]), Rules),
	recoverUnifiers(Supports, Body, [IDPremisesB, RuleB, RuleHeadB, Info], UnifiedBody),
	member([unless, RuleHeadA], UnifiedBody).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines(_, _, [IDPremisesA, RuleA, RuleHeadA, _], [[IDPremiseB], none, RuleHeadB, Info]) :-
	Info \== [[], [], []],
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines(Rules, Supports, [IDPremisesA, none, RuleHeadA, _], [IDPremisesB, RuleB, RuleHeadB, Info]) :-
	RuleB \== none,
	member(rule([RuleB, Body, _]), Rules),
	recoverUnifiers(Supports, Body, [IDPremisesB, RuleB, RuleHeadB, Info], UnifiedBody),
	member([unless, RuleHeadA], UnifiedBody).

%------------------------------------------------------------------------
% Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts(_, _, [_, _, [undercut(RuleB)], _], [_, RuleB, _, [[RuleB], _, _]]).

%------------------------------------------------------------------------
% Given a not instantiated rule and an argument, grounds the rule body using the argument support
%------------------------------------------------------------------------
recoverUnifiers(Supports, Body, Argument, FreshBody) :-
    copy_term(Body, FreshBody),
	findall(X, member(([_, _, X, _], Argument), Supports), ArgSupports),
	unifySupports(FreshBody, ArgSupports).

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
