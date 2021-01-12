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
