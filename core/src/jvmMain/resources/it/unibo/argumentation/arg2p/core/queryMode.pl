% computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
%     computeGlobalAcceptance([STATIN, STATOUT, STATUND], [_, _, _]),
%     findall(Goal, answerSingleQuery(Goal, STATIN), YesResult),
%     findall(Goal, answerSingleQuery(Goal, STATOUT), NoResult),
%     findall(Goal, answerSingleQuery(Goal, STATUND), UndResult).

computeStatementAcceptance(Goal, [Goal], [], []) :- query(Goal).
computeStatementAcceptance(Goal, [], [Goal], []) :- \+ query(Goal).

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

% Vanno propagati gli argomenti già usati per detectare i cicli
% Se non si puo costruire un argomento si deve avere UND

% OK - Posso costruire un argomento con tale claim e difenderlo
query(Query) :-
    check_modifiers_in_list(effects, [Query], [X]),
    buildArgument(X, Argument),
    defend(Argument, []).

% OK - Se non esiste on attaccante o l'attaccante è out
% NOT OK - Se non rispetta le condizioni sopra
% UND - Se c'è un ciclo
defend(Argument, QueryChain, und) :- detectCycle(Argument, QueryChain).
defend(Argument, QueryChain, Res) :-
    \+ detectCycle(Argument, QueryChain),
    \+ (
        findAttacker(Argument, Attacker),
        \+ isOut(Attacker, [Argument|QueryChain])
    ).
defend(Argument, QueryChain, no).

% OK - Se esiste un attaccante difendibile
isOut(Argument, QueryChain, und) :- detectCycle(Argument, QueryChain).
isOut(Argument, QueryChain, yes) :-
    \+ detectCycle(Argument, QueryChain),
    findAttacker(Argument, Attacker),
    defend(Attacker, [Argument|QueryChain], yes).
isOut(Argument, QueryChain, no) :- 
    \+ detectCycle(Argument, QueryChain).

% Considero le mie parti attaccabili
%  - Rules per undercut e rebut
%  - Premises per undercut e undermine
%  - Attenzione al corpo delle regole, per contrary-undercut e contrary-undermine
findAttacker([Rules, _, _], Attacker) :-
    member(X, Rules),
    \+ strict(X),
    attacker(X, Attacker).

detectCycle(Attacker, Chain) :- member(Attacker, Chain).

% undercut
attacker(Rule, Argument) :-
    buildArgument([challenge(Rule)], Argument).

% rebut e undermine
attacker(Rule, Argument) :-
    (rule([Rule, _, Conclusion]); premise([Rule, Conclusion])),
    conflict(Conclusion, X),
    buildArgument(X, Argument).

% contrary-rebut e contrary-undermine
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
build(Conclusion, [Id|Rules]) :-
    rule([Id, Premises, Conclusion]),
    findall(X, (member(P, Premises), build(P, X)), Rules).
build([prolog(_)], []).
build([unless, _], []).
