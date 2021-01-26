% Vanno propagati gli argomenti già usati per detectare i cicli in teoria

% OK - Posso costruire un argomento con tale claim e difenderlo
query(Query) :-
    check_modifiers_in_list(effects, [Query], [X])
    buildArgument(X, Argument),
    defend(Argument).

% OK - Se non esiste on attaccante o l'attaccante è out
defend(Argument) :-
    \+ (
        findAttacker(Argument, Attacker),
        \+ isOut(Attacker)
    ).

% OK - Se esiste un attaccante difendibile
isOut(Argument) :-
    findAttacker(Argument, Attacker),
    defend(Argument).

% Considero le mie parti attaccabili
%  - Rules per undercut e rebut
%  - Premises per undercut e undermine
%  - Attenzione al corpo delle regole, per contrary-undercut e contrary-undermine
findAttacker(Argument, Attacker)
    member(X, Rules),
    \+ strict(X),
    attacker(X, Attacker).

% undercut
attacker(Rule, Argument) :-
    buildArgument(challenge(Rule), Argument).

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
