computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    queryMode,
    check_modifiers_in_list(effects, [Goal], [X]),
    findall([X, Res], query(X, Res), Result),
    populateResultSets(Result, YesResult, NoResult, UndResult), !.

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
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

populateResultSets([[Query,yes]|T], [Query|Yes], No, Und) :- populateResultSets(T, Yes, No, Und).
populateResultSets([[Query,no]|T], Yes, [Query|No], Und) :- populateResultSets(T, Yes, No, Und).
populateResultSets([[Query,und]|T], Yes, No, [Query|Und]) :- populateResultSets(T, Yes, No, Und).

query(Query, Res) :-
    write(Query),nl,
    buildArgument(Query, Argument),
    write(Argument),nl,
    defend(Argument, [], Res).
query(_, und).

% Caso 3 - Un attaccante è difendibile
defend(Argument, QueryChain, no) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == yes.
% Caso 1 - Ciclo fra gli attaccanti -> UND
defend(Argument, QueryChain, und) :- 
    findAttacker(Argument, QueryChain, Attacker, no).
% Caso 4 - La valutazione di uno degli attaccanti fin'ora validi darà corso ad un ciclo
defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    isOut(Attacker, [Argument|QueryChain], und).
% Caso 2 - Tutti gli attaccanti, se esistenti, sono OUT -> IN
defend(Argument, QueryChain, yes) :-
    \+ (
        findAttacker(Argument, QueryChain, Attacker, yes),
        \+ isOut(Attacker, [Argument|QueryChain], yes)
    ).

% OK - Se esiste un attaccante difendibile
isOut(Argument, QueryChain, Res) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    defend(Attacker, [Argument|QueryChain], Res),
    (Res == yes; Res == und).
isOut(Argument, QueryChain, und) :- 
    \+ findAttacker(Argument, QueryChain, _, yes),
    findAttacker(Argument, QueryChain, Attacker, no).

% Considero le mie parti attaccabili
%  - Rules per undercut e rebut
%  - Premises per undercut e undermine
%  - Attenzione al corpo delle regole, per contrary-undercut e contrary-undermine
findAttacker([Rules, _, _], QueryChain, Attacker, IsValid) :-
    member(X, Rules),
    \+ strict(X),
    attacker(X, Attacker),
    findAttackerCicle(Attacker, QueryChain, IsValid).

findAttackerCicle(Attacker, QueryChain, yes) :- \+ member(Attacker, QueryChain).
findAttackerCicle(Attacker, QueryChain, no) :- member(Attacker, QueryChain).

% undercut
attacker(Rule, Argument) :-
    buildArgument([challenge(Rule)], Argument).

% rebut e undermine
attacker(Rule, Argument) :-
    (rule([Rule, _, Conclusion]); premise([Rule, Conclusion])),
    conflict(Conclusion, X),
    buildArgument(X, Argument).

% contrary-rebut and contrary-undermine
attacker(Rule, Argument) :-
    rule([Rule, Premises, _]),
    member([unless, X], Premises),
    buildArgument(X, Argument).

% Ricorsione sul corpo di 
%   - rule([id, [premises], conclusion])
%   - premise([id, conclusion])
% fino ad arrivare a una regola senza premesse o ad una premessa 
buildArgument(Query, Argument) :-
    build(Query, [TopRule|Rules]),
    Argument = [[TopRule|Rules], TopRule, Query].
    
build(Conclusion, [Id]) :-
    premise([Id, Conclusion]).
build(Conclusion, [Id|Res]) :-
    rule([Id, Premises, Conclusion]),
    buildPremises(Premises, Res).
build([prolog(_)], []).
build([unless, _], []).

buildPremises([], []).
buildPremises([X|T], RR) :-
    build(X, Rules),
    buildPremises(T, Res),
    appendLists([Rules, Res], RR).
