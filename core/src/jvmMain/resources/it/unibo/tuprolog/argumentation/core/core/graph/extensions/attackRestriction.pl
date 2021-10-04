modifyArgumentationGraph([Arguments, Attacks, Supports], [Arguments, NewAttacks, Supports]) :-
    checkRebutRestrictions(Attacks, NewAttacks).

checkRebutRestrictions([], []).
checkRebutRestrictions([Attack|Attacks], NewAttacks) :-
    checkRebutRestrictions(Attacks, TempAttacks),
	checkRebutRestriction(Attack, R),
	utils::appendLists([R, TempAttacks], NewAttacks).

checkRebutRestriction((T, A, B, C), [(T, A, B, C)]) :-
    rebutRestriction(T, A, B, C), !.
checkRebutRestriction((T, A, B, C), []) :-
    \+ rebutRestriction(T, A, B, C).

rebutRestriction(rebut, _, _, C) :- restrict(C).
rebutRestriction(undermine, _, _, C).
rebutRestriction(contrary_rebut, _, _, _).
rebutRestriction(contrary_undermine, _, _, _).
rebutRestriction(undercut, _, _, _).

restrict([_, TopRule, _, [[TopRule], _, _]]) :- TopRule \== none.
