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
        retractall(attack(_, _, _)),
	    retractall(support(_, _)),
	    buildArguments,
        buildAttacks,
        findall( [IDPremises,  TopRule,  RuleHead],
                 ( argument([IDPremises, TopRule, RuleHead]),
                   ground(argument([IDPremises, TopRule, RuleHead])) ),
                 Arguments),
        findall( (A1, A2), support(A1, A2), Supports),
	    findall( (T, A1, A2), attack(T, A1, A2),  Attacks),
        printArgumentationGraph, !.

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

% Per ogni regola, vedo le premesse, controllo che ci siano degli argomenti
% a favore degli statements contenuti nel corpo della regola,
% se si gli aggiungo al supporto dell'argomento.
% Se l'argomento è nuovo lo aggiungo alla teoria

buildArguments :-
	premise([PremiseID, Premise]),
	NewArgument = [[PremiseID], none, Premise],
	\+ argument(NewArgument),
	asserta(argument(NewArgument)),
	asserta(prem([PremiseID, Premise], NewArgument)),
    fail.
buildArguments :-
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
    buildArguments.

buildArguments.

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
ruleBodyIsSupported([ Statement | Others], Premises, Supports, ResultPremises, ResultSupports) :-
    argument([ArgumentID, RuleID, Statement]),
	append(ArgumentID, Premises, NewPremises),
	append([[ArgumentID, RuleID, Statement]], Supports, NewSupports),
	ruleBodyIsSupported(Others, NewPremises, NewSupports, ResultPremises, ResultSupports).

assertSupports([], _).
assertSupports([Support | OtherSupports], Argument) :-
	asserta(support(Support, Argument)),
	assertSupports(OtherSupports, Argument).

liftPremises(Supports, Argument) :-
	findall(_, (member(S, Supports), prem(Prem, S), asserta(prem(Prem, Argument))), _).

%========================================================================
% ATTACK DEFINITION
%========================================================================

buildAttacks :-
	argument(A),
	argument(B),
	A \== B,
    attacks(T, A, B),
	\+(attack(T, A, B)),
	asserta(attack(T, A, B)),
	fail.

buildAttacks :-
	attack(T, A, B),
	support(B, C),
	acceptableTransitivity(T, A, C),
	\+ attack(T, A, C),
	asserta(attack(T, A, C)),
    buildAttacks.

buildAttacks.

attacks(rebut, A, B) :- rebuts(A, B).
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B).
attacks(undermine, A, B) :- undermines(A, B).
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B).
attacks(undercut, A, B) :- undercuts(A, B).

acceptableTransitivity(rebut, A, B) :- restrict(A, B), \+ superiorArgument(B, A).
acceptableTransitivity(contrary_rebut, A, B) :- restrict(A, B).
acceptableTransitivity(undermine, A, B) :- \+ superiorArgument(B, A).
acceptableTransitivity(contrary_undermine, _, _).
acceptableTransitivity(undercut, _, _).

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

%------------------------------------------------------------------------
% Sub argument definition: structures that support intermediate conclusions
% (plus the argument itself and its premises as limiting cases)
% Given an argument A : A1...An; j1; the set of its subarguments
% Sub(A), the set of its direct subarguments DirectSub(A),are defined as follows:
% • Sub(A) = Sub(A1) U ... U Sub(An) U {A},
% • DirectSub(A) = {A1,...An}
%------------------------------------------------------------------------
sub(B, [B |Subs]) :-
	findall(Sub,  support(Sub, B), Subs ).

ordinaryPremises(B, Prem) :-
	findall(R,  (prem([R, _], B), \+ strict(R)), Prem).

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) :-
	RuleB \== none,
	\+ strict(RuleB),
	conflict(RuleHeadA, RuleHeadB),
	\+ superiorArgument([IDPremisesB, RuleB, RuleHeadB], [IDPremisesA, RuleA, RuleHeadA]).

%------------------------------------------------------------------------
% Contrary Rebutting definition: clash of a conclusion with a failure as premise assumption
%------------------------------------------------------------------------
contraryRebuts([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) :-
	RuleA \== none,
	RuleB \== none,
	\+ strict(RuleB),
	rule([RuleB, Body, _]),
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines([IDPremisesA, RuleA, RuleHeadA], [[IDPremiseB], none, RuleHeadB]) :-
	\+ strict(IDPremiseB),
	conflict(RuleHeadA, RuleHeadB),
	\+ superiorArgument([[IDPremiseB], none, RuleHeadB], [IDPremisesA, RuleA, RuleHeadA]).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines([IDPremisesA, none, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) :-
	RuleB \== none,
	\+ strict(RuleB),
	rule([RuleB, Body, _]),
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([_, _, [challenge(RuleB)]], [_, RuleB, _]) :-
	\+ strict(RuleB).

%------------------------------------------------------------------------
% Rebut restriction. If the attacked argument has a strict rule as 
% the TopRule also the attacker must
%------------------------------------------------------------------------
restrict(_, _) :- unrestrictedRebut.
restrict([_, TopRuleA, _], [_, TopRuleB, _ ]) :-
	\+ unrestrictedRebut,
	\+ (strict(TopRuleB), \+ strict(TopRuleA)).

%------------------------------------------------------------------------
% Superiority definition
% A superiority relation over a set of rules Rules is an antireflexive and
% antisymmetric binary relation over Rules
%------------------------------------------------------------------------

superiorArgument([RulesA, TopRuleA, ConcA], [RulesB, TopRuleB, ConcB]) :-
	influentRules(RulesA, TopRuleA, ConcA, InfluentA),
	influentRules(RulesB, TopRuleB, ConcB, InfluentB), !,
	ordinaryPremises([RulesA, TopRuleA, ConcA], PremisesA),
	ordinaryPremises([RulesB, TopRuleB, ConcB], PremisesB),
	superior(InfluentA, PremisesA, InfluentB, PremisesB).

superior([], PremisesA, [], PremisesB) :-
	weaker(PremisesB, PremisesA).
superior(DefRulesA, _, DefRulesB, _) :-
	orderingPrinciple(last),
	(DefRulesA \== []; DefRulesB \== []),
	weaker(DefRulesB, DefRulesA).
superior(DefRulesA, [], DefRulesB, []) :-
	orderingPrinciple(weakest),
	weaker(DefRulesB, DefRulesA).
superior(DefRulesA, PremisesA, DefRulesB, PremisesB) :-
	orderingPrinciple(weakest),
	(DefRulesA \== []; DefRulesB \== []),
	(PremisesA \== []; PremisesB \== []),
	weaker(DefRulesB, DefRulesA),
	weaker(PremisesB, PremisesA).


influentRules(Rules, _, _, InfluentRules) :-
	orderingPrinciple(weakest),
	weakestRule(Rules, InfluentRules).

influentRules(Rules, TopRule, Conc, InfluentRules) :-
	orderingPrinciple(last),
	lastRule(Rules, TopRule, Conc, InfluentRules).

weakestRule(Rules, Influent) :-
	findall(X, (member(X, Rules), \+ strict(X), \+ premise([X, _])), Influent).

lastRule(Rules, none, Conc, []).
lastRule(Rules, TopRule, Conc, [TopRule]) :- TopRule \== none, \+ strict(TopRule).
lastRule(Rules, TopRule, Conc, Influent) :- 
	strict(TopRule),
	findall(X, (support([R, TR, C], [Rules, TopRule, Conc]), lastRule(R, TR, C, X)), Res),
	flatten(Res, Influent).

weaker(RulesA, []) :-
	RulesA \== [].

weaker(RulesA, RulesB) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(elitist),
	member(Rule, RulesA),
	allStronger(Rule, RulesB), !.

weaker(RulesA, RulesB) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(democrat),
	weakerDemo(RulesA, RulesB).


%(A, B) ∈ attnr(K) iff 1. A undercuts B, or 2. A rebuts B (at B′) 
% and there is no defeasible rule d ∈ ldr(A) such that d ≺ last(B′).
weaker(RulesA, RulesB) :-
	RulesA \== [],
	RulesB \== [],
	orderingComparator(normal),
	member(W, RulesA),
	member(X, RulesB),
	sup(X, W), !.

weakerDemo([], _).
weakerDemo([H|T], Rules) :-
	singleStronger(H, Rules),
	weakerDemo(T, Rules).

allStronger(_, []).
allStronger(Target, [Rule|Rules]) :-
	sup(Rule, Target),
	allStronger(Target, Rules).

singleStronger(Target, Rules) :-
	member(Rule, Rules),
	sup(Rule, Target), !.
