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
buildArgumentationGraph([Arguments, Attacks, Supports] ) :-
        retractall(argument(_)), !,
        retractall(attack(_, _)), !,
	    retractall(support(_, _)), !,
	    buildArguments,
        buildAttacks,
        findall( [IDPremises,  TopRule,  RuleHead],
                 ( argument([IDPremises, TopRule, RuleHead]),
                   ground(argument([IDPremises, TopRule, RuleHead])) ),
                 Arguments),
        findall( (A1, A2), support(A1, A2), Supports),
	    findall( (A1, A2), attack(A1, A2),  Attacks),
        printArgumentationGraph.

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
	rule([RuleID, RuleBody, RuleHead]),
	ruleBodyIsSupported(RuleBody, [], [], PremisesOfSupportingArguments, Supports),
	append([RuleID], PremisesOfSupportingArguments, IDPremises),
	sort(IDPremises, SortedPremises), % Also remove duplicates
    NewArgument = [SortedPremises, RuleID, RuleHead],
	\+ argument(NewArgument),
	assertSupports(Supports, NewArgument),
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
ruleBodyIsSupported([ Statement | Others], Premises, Supports, ResultPremises, ResultSupports) :-
    argument([ArgumentID, RuleID, Statement]),
	append(ArgumentID, Premises, NewPremises),
	append([[ArgumentID, RuleID, Statement]], Supports, NewSupports),
	ruleBodyIsSupported(Others, NewPremises, NewSupports, ResultPremises, ResultSupports).


assertSupports([], _).
assertSupports([Support | OtherSupports], Argument) :-
	asserta(support(Support, Argument)),
	assertSupports(OtherSupports, Argument).

%========================================================================
% ATTACK DEFINITION
%========================================================================
%Definition: An attack relation;over a set of arguments A is a binary relation over A ,
%i.e. ; A A . An argument B attacks an argument A, i.e. B;A, iff B rebuts or undercuts
%A, where
%• B rebuts A (on A0) iff exists A0 in Sub(A) such that conc(B) and conc(A0) are in conflict, i.e.
%Conflicts(conc(B);conc(A0)), and A0 not superior B;
%• B undercuts A (on A0) iff exists A0 in Sub(A) such that  conc(B) belongs to the body of
%TopRule(A0), i.e. ( conc(B)) in Body(TopRule(A0)).
%========================================================================

% controllo sempre in cascata

% -- REBUTS --
% prendo due argomenti A e B
% di B ottengo il supporto
% controllo che la conclusione dei membri del supporto non sia in conflitto con la conclusione di A
% se lo è controllo se B è superiore ad A
% in caso affermativo, se già non l'ho inserito, aggiungo l'attacco alla teoria

buildAttacks :-
	argument([IDPremisesA, RuleA, RuleHeadA]),
	argument([IDPremisesB, RuleB, RuleHeadB]),
    sub([IDPremisesB, RuleB, RuleHeadB], Subs),
    member([IDPremisesSubB, RuleSubB, RuleHeadSubB], Subs),

    conflict(RuleHeadA, RuleHeadSubB),
    rebuts([IDPremisesA, RuleA, RuleHeadA], [IDPremisesSubB, RuleSubB, RuleHeadSubB]),

	\+( attack([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) ),
	asserta( attack([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) ),
	fail.

buildAttacks :-
	argument([IDPremisesA, RuleA, RuleHeadA]),
	argument([IDPremisesB, RuleB, RuleHeadB]),
    undercuts([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]),
	\+( attack([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) ),
	asserta( attack([IDPremisesA, RuleA, RuleHeadA], [IDPremisesB, RuleB, RuleHeadB]) ),
	fail.

/*
    Attacchi transitivi
*/
buildAttacks :-
	attack(A, B),
	support(B, C),
	\+ attack(A, C),
	asserta( attack(A, C)),
    buildAttacks.

buildAttacks.

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

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts(A, B) :-
        \+ superiorArgument(B, A).

%------------------------------------------------------------------------
% Superiority definition
% A superiority relation over a set of rules Rules is an antireflexive and
% antisymmetric binary relation over Rules
%------------------------------------------------------------------------
superiorArgument([_, TopRuleA, _], [_, TopRuleB, _ ]) :-
        sup(TopRuleA, TopRuleB).

%------------------------------------------------------------------------
% Undercutting definition: attacks on negation as failure premises
%------------------------------------------------------------------------
undercuts([_, _, RuleHeadA], [_, RuleB, _]) :-
        rule([RuleB, Body, _]),
        member([unless, RuleHeadA], Body).
