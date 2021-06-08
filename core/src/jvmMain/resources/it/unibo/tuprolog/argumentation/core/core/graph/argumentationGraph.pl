%========================================================================
% ARGUMENTATION GRAPH
%========================================================================

buildArgumentationGraph([Arguments, Attacks, Supports]) :-
        retractall(argument(_)),
        retractall(argumentInfo(_, _)),
		retractall(attack(_, _, _, _)),
        retractall(attack(_, _, _)),
	    retractall(support(_, _)),
	    buildArguments,
        buildAttacks,
        findall( [IDPremises,  TopRule,  RuleHead],
                 ( argument([IDPremises, TopRule, RuleHead]),
                   ground(argument([IDPremises, TopRule, RuleHead])) ),
                 Arguments),
        findall( (A1, A2), support(A1, A2), Supports),
	    findall( (T, A1, A2), attack(T, A1, A2),  Attacks), !.

%========================================================================
% ARGUMENT DEFINITION
%========================================================================

buildArguments :-
	buildArgumentsFromPremises,
	buildArgumentsFromRules,
	buildArgumentsInfo.

buildArgumentsFromPremises :-
	premise([PremiseID, Premise]),
	NewArgument = [[PremiseID], none, Premise],
	\+ argument(NewArgument),
	asserta(argument(NewArgument)),
	asserta(prem([PremiseID, Premise], NewArgument)),
    fail.
buildArgumentsFromPremises.

buildArgumentsFromRules :-
	rule([RuleID, RuleBody, RuleHead]),
	ruleBodyIsSupported(RuleBody, [], [], PremisesOfSupportingArguments, Supports),
	\+ member(RuleID, PremisesOfSupportingArguments),
	append([RuleID], PremisesOfSupportingArguments, IDPremises),
	sort(IDPremises, SortedPremises),
    NewArgument = [SortedPremises, RuleID, RuleHead],
	\+ argument(NewArgument),
	assertSupports(Supports, NewArgument),
	liftPremises(Supports, NewArgument),
	asserta(argument(NewArgument)),
    buildArgumentsFromRules.
buildArgumentsFromRules.

buildArgumentsInfo :-
	argument(Argument),
	defeasibleRules(Argument, Rules),
	ordinaryPremises(Argument, Prem),
	lastDefeasibleRules(Argument, LastRules),
	\+ argumentInfo(Argument, [LastRules, Rules, Prem]),
	asserta(argumentInfo(Argument, [LastRules, Rules, Prem])),
	fail.
buildArgumentsInfo.

assertSupports([], _).
assertSupports([Support | OtherSupports], Argument) :-
	asserta(support(Support, Argument)),
	assertSupports(OtherSupports, Argument).

liftPremises(Supports, Argument) :-
	findall(_, (member(S, Supports), prem(Prem, S), asserta(prem(Prem, Argument))), _).

% Sub-arguments
sub(A, [A|Subs]) :- findall(Sub, support(Sub, A), Subs).

% Defeasible Premises
ordinaryPremises(A, Prem) :- findall(R,  (prem([R, _], A), \+ strict(R)), Prem).

% Defeasible rules
defeasibleRules([Rules, _, _], DefRules) :- 
	findall(X, (member(X, Rules), \+ strict(X), \+ premise([X, _])), UnsortedRules),
	sort(UnsortedRules, DefRules).

% Last Defeasible Rules
lastDefeasibleRules(A, Rules) :- lastRule(A, Rules).
	
lastRule([Rules, none, Conc], []).
lastRule([Rules, TopRule, Conc], [TopRule]) :- TopRule \== none, \+ strict(TopRule).
lastRule([Rules, TopRule, Conc], Influent) :- 
	strict(TopRule),
	findall(X, (support([R, TR, C], [Rules, TopRule, Conc]), lastRule([R, TR, C], X)), Res),
	appendLists(Res, Influent).

%========================================================================
% SUPPORT DEFINITION
%========================================================================

ruleBodyIsSupported([], ResultPremises, ResultSupports, ResultPremises, ResultSupports).
ruleBodyIsSupported([ [unless, _] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([ [prolog(Check)] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([Statement|Others], Premises, Supports, ResultPremises, ResultSupports) :-
    argument([ArgumentID, RuleID, Statement]),
	append(ArgumentID, Premises, NewPremises),
	append([[ArgumentID, RuleID, Statement]], Supports, NewSupports),
	ruleBodyIsSupported(Others, NewPremises, NewSupports, ResultPremises, ResultSupports).

%========================================================================
% ATTACK DEFINITION
%========================================================================

buildAttacks :-
	buildDirectAttacks,
	buildTransitiveAttacks.

buildDirectAttacks :-
	argument(A),
	argument(B),
	A \== B,
    once(attacks(T, A, B)),
	\+ attack(T, A, B, B),
	asserta(attack(T, A, B)),
	asserta(attack(T, A, B, B)),
	fail.
buildDirectAttacks.

buildTransitiveAttacks :-
	attack(T, A, B, D),
	support(B, C),
	\+ attack(T, A, C, D),
	asserta(attack(T, A, C)),
	asserta(attack(T, A, C, D)),
    buildTransitiveAttacks.
buildTransitiveAttacks.

% Attack definition
attacks(rebut, A, B) :- rebuts(A, B).
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B).
attacks(undermine, A, B) :- undermines(A, B).
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B).
attacks(undercut, A, B) :- undercuts(A, B).

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

%------------------------------------------------------------------------
% Check if the given argument is strict
%------------------------------------------------------------------------
strictArgument(Argument) :- argumentInfo(Argument, [_, [], []]).

%========================================================================
% CONFLICT DEFINITION
%========================================================================
% Literals statments have the following form.
% literals: [atom] or [neg, atom]
% deontic literals: [obl, [atom]] or [obl, [neg, atom]] or [neg, obl,[neg, atom]] or [perm, [neg, atom]]
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
