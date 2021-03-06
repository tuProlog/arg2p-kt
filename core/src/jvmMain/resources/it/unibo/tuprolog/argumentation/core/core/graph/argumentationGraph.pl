% ----------------------------------------------------------------------
% argumentationGraph.pl
%
% PIKA-LAB
% Year: 2019
% ----------------------------------------------------------------------

%========================================================================
% ARGUMENTATION GRAPH
%========================================================================
% The argumentation graph consists of a finite set A called arguments and
% two binary relations on A called attack and support respectively.
% The argumentation graph is build based on the following definition:
% - ARGUMENT
% - ATTACK and CONFLICT
% - SUPPORT
% answering the following questions
% - how can arguments be built, i.e. how can claims be supported with grounds,
% - and how can arguments be attacked
%========================================================================
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
% Arguments can be constructed step-by-step by chaining inference rules into trees.
% Arguments thus contain subarguments, which are the structures that support
% intermediate conclusions (plus the argument itself and its premises as limiting cases).
% Arguments are then defined as a chain applications of the inference rules into inference trees,
% starting with elements from the knowledge base K. In what follows, for a given argument,
% the function Prem returns all the formulas of K (called premises) used to build the argument,
% Conc returns its conclusion, Sub returns all its sub-arguments,
% DefRules returns all the defeasible rules of the argument and TopRule returns the last inference
% rule used in the argument.
% An argument A on the basis of an argumentation theory with a knowledge base K
% and an argumentation system (L, R, n) is
% (1) φ if φ ∈ K with: Prem(A) = {φ},Conc(A) = φ,Sub(A) = {φ},DefRules(A) = ∅,TopRule(A) = undefined.
% (2) A1,...An ⇒ ψ if A1,...,An are arguments such that there exists a defeasible rule Conc(A1), . . . ,Conc(An) ⇒ ψ in Rd.
%     Prem(A) = Prem(A1 ) ∪ . . . ∪ Prem(An ),
%     Conc(A) = ψ,
%     Sub(A) = Sub(A1) ∪ . . . ∪ Sub(An) ∪ {A},
%     DefRules(A) = DefRules(A1 ) ∪ . . . ∪ DefRules(An ) ∪ {Conc(A1 ), . . .
%     Conc(An) ⇒ ψ},
%     TopRule(A) = Conc(A1 ), . . . Conc(An ) ⇒ ψ .
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
% means that argument a supports argument b if the acceptance of a implies the acceptance of b
% support are inferences from grounds to claims
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

