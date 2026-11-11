% A clause among X may chain with the rules of the theory: its premises are concluded by a rule, or its conclusion is a
% premise of a rule
chainable(X) :-
    member(Head :- Body, X),
    store::ruleClause(_, Premises, _, Head, Body),
    store::premiseList(Premises, List),
    member(Premise, List),
    store::countConcluding(Premise, 1, Count),
    Count > 0.
chainable(X) :-
    member(Head :- Body, X),
    store::ruleClause(_, _, Conclusion, Head, Body),
    usedByARule(Conclusion).
chainable(X) :-
    member(Head :- Body, X),
    store::premiseClause(_, Conclusion, Head, Body),
    usedByARule(Conclusion).

usedByARule(Conclusion) :-
    store::rule(_, Premises, _),
    member(Conclusion, Premises).
