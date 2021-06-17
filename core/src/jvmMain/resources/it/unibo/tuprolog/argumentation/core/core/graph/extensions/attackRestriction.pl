modifyArgumentationGraph(rebutRestriction, [Arguments, Attacks, Supports], [Arguments, NewAttacks, Supports]) :-
    checkRebutRestrictions(Attacks, NewAttacks), !.

checkRebutRestrictions([], []).
checkRebutRestrictions([Attack|Attacks], NewAttacks) :-
    checkRebutRestrictions(Attacks, TempAttacks),
	checkRebutRestriction(Attack, R),
	appendLists([R, TempAttacks], NewAttacks).

checkRebutRestriction((T, A, B),[(T, A, B)]) :-
    attack(T, A, B, C),
    rebutRestriction(T, A, B, C), !.
checkRebutRestriction((T, A, B),[]) :-
    attack(T, A, B, C),
    \+ rebutRestriction(T, A, B, C),
    retractall(attack(T, A, B, C)),
    retractall(attack(T, A, B)).

rebutRestriction(rebut, _, _, C) :- restrict(C).
rebutRestriction(undermine, _, _, C) :- restrict(C).
rebutRestriction(contrary_rebut, _, _, _).
rebutRestriction(contrary_undermine, _, _, _).
rebutRestriction(undercut, _, _, _).

%------------------------------------------------------------------------
% Rebut/Undermine restriction.
%------------------------------------------------------------------------

restrict([_, TopRule, _ ]) :- TopRule \== none, \+ strict(TopRule).
restrict([[Premise], none, _ ]) :- \+ strict(Premise).
