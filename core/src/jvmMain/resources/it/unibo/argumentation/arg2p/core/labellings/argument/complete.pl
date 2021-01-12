argumentCompleteLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]) :-
    retractall(compl(_,_,_)),
    grounded(Arguments, Attacks, [], [], Arguments, RAttacks, RIN, ROUT, RUND),
    completeLabellingT(Arguments, RAttacks, RIN, ROUT, RUND, _, IN, OUT, UND),
    cleanTempSup.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


completeLabellingT(_, Attacks, IN, OUT, UND, Attacks, IN, OUT, UND) :- 
    admissible(IN, IN, Attacks),
    sort(IN, SortIN),
    sort(OUT, SortOUT),
    sort(UND, SortUND),
    \+ compl(SortIN, SortOUT, SortUND),
    assert(compl(SortIN, SortOUT, SortUND)).
completeLabellingT(Arguments, Attacks, IN, OUT, UND, ResultAttacks, ResultIN, ResultOUT, ResultUND) :-
    member(X, UND),
    subtract(UND, [X], NewUND),
    grounded(Arguments, Attacks, [X|IN], OUT, NewUND, RAttacks, RIN, ROUT, RUND),
    completeLabellingT(Arguments, RAttacks, RIN, ROUT, RUND, ResultAttacks, ResultIN, ResultOUT, ResultUND).


grounded(Arguments, Attacks, IN, OUT, UND, ResultAttacks, ResultIN, ResultOUT, ResultUND) :-
    groundedLabelling(Arguments, Attacks, IN, OUT, UND, ResultAttacks, ResultIN, ResultOUT, ResultUND), !.


admissible(_, [], _).
admissible(IN, [H|T], Attacks) :-
    \+ (member((_, Attacker, H), Attacks), 
        \+ ( member((_, Defendant, Attacker), Attacks), member(Defendant, IN))),
    admissible(IN, T, Attacks).

/*
    Pick 0, 1, N arguments under these conditions:
        - the resulting set must be conflict-free
*/
% findConflictFreeSet([], _, _, []).
% findConflictFreeSet([H|T], Attacks, Supports, [H|T2]) :-
%     findConflictFreeSet(T, Attacks, Supports, T2),
%     \+ (member((_, H, Y), Attacks), member(Y, T2)),
%     \+ (member((_, Z, H), Attacks), member(Z, T2)).
% findConflictFreeSet([_|T], Attacks, Supports, T2) :- findConflictFreeSet(T, Attacks, Supports, T2).
