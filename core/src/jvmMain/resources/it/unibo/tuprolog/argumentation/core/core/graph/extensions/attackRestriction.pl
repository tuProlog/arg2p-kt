modifyArgumentationGraph :-
    findall(_, (
        context_check(attack(T, A, B, C)),
        checkRebutRestriction(T, A, B, C)
    ), _).

checkRebutRestriction(T, A, B, C) :-
    \+ rebutRestriction(T, A, B, C),
    standard_af::removeAttack(T, A, B, C).

rebutRestriction(rebut, _, _, C) :- restrict(C).
rebutRestriction(undermine, _, _, C).
rebutRestriction(contrary_rebut, _, _, _).
rebutRestriction(contrary_undermine, _, _, _).
rebutRestriction(undercut, _, _, _).

restrict([_, TopRule, _, _, [[TopRule], _, _]]) :- TopRule \== none.
