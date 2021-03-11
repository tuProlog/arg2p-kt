computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    queryMode,
    check_modifiers_in_list(effects, [Goal], [X]),
    findall([X, Res], query(X, Res), Result),
    populateResultSets(Result, YesResult, NoResult, UndResult), !.

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    \+ queryMode,
    computeGlobalAcceptance([STATIN, STATOUT, STATUND], [_, _, _]),
    findall(Goal, answerSingleQuery(Goal, STATIN), YesResult),
    findall(Goal, answerSingleQuery(Goal, STATOUT), NoResult),
    findall(Goal, answerSingleQuery(Goal, STATUND), UndResult).

answerSingleQuery(Goal, Set) :-
    check_modifiers_in_list(effects, [Goal], [X]),
    member(X, Set).

isSkepticallyAcceptable(Goal) :-
    convertAllRules,
    findall(STATIN, computeGlobalAcceptance([STATIN, _, _], [_, _, _]), SOLUTIONS),
    check_modifiers_in_list(effects, [Goal], [X]),
    all(X, SOLUTIONS), !.

all(_, []).
all(Goal, [H|T]) :- member(Goal, H), all(Goal, T).

isCredulouslyAcceptable(Goal) :-
    convertAllRules,
    computeGlobalAcceptance([STATIN, _, _], [_, _, _]),
    answerSingleQuery(Goal, STATIN), !.

populateResultSets([], [], [], []).
populateResultSets([[Query,yes]|T], [Query|Yes], No, Und) :- populateResultSets(T, Yes, No, Und).
populateResultSets([[Query,no]|T], Yes, [Query|No], Und) :- populateResultSets(T, Yes, No, Und).
populateResultSets([[Query,und]|T], Yes, No, [Query|Und]) :- populateResultSets(T, Yes, No, Und).

query(Query, Res) :-
    buildArgument(Query, Argument),
    write(Argument),nl,
    once(defend(Argument, [], Res)).
query(Query, und) :- \+ buildArgument(Query, _).

% Poco ottimizzato: mentre cerco un In potrei già trovare un Und (va bufferizzato)
% Caso base: esiste fra i miei attaccanti un verificato -> io sono out
defend(Argument, QueryChain, no) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == yes, !.
% Caso ciclo: esiste fra i miei attaccanti un argomento già incontrato (ciclo) sono Und
defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, no), !.
% Caso indeterminatezza: con solo un attaccante Und sono Und
defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == und, !.
% Negli altri casi sono In
defend(Argument, QueryChain, yes).

% Considero le mie parti attaccabili
%  - Rules per undercut e rebut
%  - Premises per undercut e undermine
%  - Attenzione al corpo delle regole, per contrary-undercut e contrary-undermine
findAttacker(Target, QueryChain, Attacker, IsValid) :-
    attacker(Target, Attacker),
    findAttackerCicle(Attacker, QueryChain, IsValid).

findAttackerCicle(Attacker, QueryChain, yes) :- \+ member(Attacker, QueryChain).
findAttackerCicle(Attacker, QueryChain, no) :- member(Attacker, QueryChain).

attacker([Rules, TopRule, Conclusion, Groundings], Argument) :-
    member(X, Rules),
    \+ strict(X),
    attackerOnRule(X, [Rules, TopRule, Conclusion, Groundings], Argument).

attacker([Rules, TopRule, Conclusion, Groundings], Argument) :-
    member(X, Groundings),
    attackerOnTerm(X, [Rules, TopRule, Conclusion, Groundings], Argument).

% undercut
attackerOnRule(Rule, _, Argument) :-
    buildArgument([undercut(Rule)], Argument).

% rebut e undermine
attackerOnTerm(Term, [TargetRules, TargetTopRule, TargetConc, _], [Rules, TopRule, Conc, Grondings]) :-
    Term \== [unless, _],
    conflict(Term, X),
    buildArgument(X, [Rules, TopRule, Conc, Grondings]),
    \+ superiorArgument([TargetRules, TargetTopRule, TargetConc], [Rules, TopRule, Conc]).

% contrary-rebut and contrary-undermine
attackerOnTerm([unless, X], _, Argument) :-
    buildArgument(X, Argument).

% Ricorsione sul corpo di 
%   - rule([id, [premises], conclusion])
%   - premise([id, conclusion])
% fino ad arrivare a una regola senza premesse o ad una premessa
% Mi porto dietro il grounding dei termini per poter derivare gli attacchi
buildArgument(Query, Argument) :-
    build(Query, [TopRule|Rules], Groundings),
    Argument = [[TopRule|Rules], TopRule, Query, Groundings].

build(Conclusion, [Id], [Conclusion]) :-
    premise([Id, Conclusion]).
build(Conclusion, [Id|Res], [Conclusion|Concls]) :-
    rule([Id, Premises, Conclusion]),
    buildPremises(Premises, Res, Concls).
build([prolog(_)], [], []).
build([unless, Atom], [], [[unless, Atom]]).

buildPremises([], [], []).
buildPremises([X|T], RR, CC) :-
    build(X, Rules, Concls),
    buildPremises(T, Res, Concls2),
    appendLists([Rules, Res], RR),
    appendLists([Concls, Concls2], CC).
