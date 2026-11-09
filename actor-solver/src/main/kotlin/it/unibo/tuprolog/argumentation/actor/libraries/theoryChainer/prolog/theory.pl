% A clause among X may chain with the rules of the theory: its premises are concluded by a rule, or its conclusion is a
% premise of a rule
chainable(X) :-
    member(Head :- Body, X),
    parser::ruleClause(_, Premises, _, Head, Body),
    parser::premiseList(Premises, List),
    member(Premise, List),
    parser::countConcluding(Premise, 1, Count),
    Count > 0.
chainable(X) :-
    member(Head :- Body, X),
    parser::ruleClause(_, _, Conclusion, Head, Body),
    usedByARule(Conclusion).
chainable(X) :-
    member(Head :- Body, X),
    parser::premiseClause(_, Conclusion, Head, Body),
    usedByARule(Conclusion).

usedByARule(Conclusion) :-
    parser::ruleWritten(_, Premises, _),
    member(Conclusion, Premises).
