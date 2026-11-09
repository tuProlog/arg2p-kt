%! subsets(?List, ?Subset)
%  Subset is each subset of List, its elements in their order.
subsets([], []).
subsets([H|T], [H|NT]) :- subsets(T, NT).
subsets([_|T], NT) :- subsets(T, NT).

%! properSubsets(+List, -Subset)
%  Subset is each subset of List other than the empty one and List itself.
properSubsets(L, S) :-
    subsets(L, S),
    S \= [],
    S \= L.

%! generateInterventions(+Rules, +Cause, -Intervention)
%  Intervention is each change of the theory removing Cause: its complement, the undercut of Cause (a rule,
%  unless it is negated), or the complement with a set of the premises of the rules not among Rules (the top
%  rules of the graph of the effect).
generateInterventions(_, S, [CS]) :-
    compl(S, CS).
generateInterventions(_, S, [undercut(S)]) :-
    S \= -_.
generateInterventions(R, S, SUP) :-
    compl(S, CS),
    unusedPremises(R, UP), !,
    subsets(UP, SUP),
    member(CS, SUP),
    SUP \= [].

%! compl(?Statement, ?Complement)
%  Complement is the complement of Statement (-X for X, X for -X).
compl(-X, X) :- !.
compl(X, -X).

%! unusedPremises(+Rules, -Premises)
%  Premises are those of the rules of the theory not among Rules.
unusedPremises(R, UP) :-
    findall(P, (parser::rule(Id, P, _), \+ member(Id, R), ground(Id)), RR),
    utils::appendLists(RR, UP).
