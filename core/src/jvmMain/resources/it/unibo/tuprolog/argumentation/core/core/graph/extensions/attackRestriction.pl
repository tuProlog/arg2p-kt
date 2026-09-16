modifyArgumentationGraph :-
    findall(_, (
        context_check(attack(T, A, B, C)),
        checkRebutRestriction(T, A, B, C)
    ), _).

checkRebutRestriction(T, A, B, C) :-
    \+ rebutRestriction(T, A, B, C),
    standard_af::removeAttack(T, A, B, C).

% The attack types the restriction does not apply to. Everything else is restricted, including the attack
% types introduced by other extensions: an unknown attack type is subject to the restriction rather than
% dropped, so that the result does not depend on the order in which the extensions run.
rebutRestriction(undermine, _, _, _).
rebutRestriction(contrary_rebut, _, _, _).
rebutRestriction(contrary_undermine, _, _, _).
rebutRestriction(undercut, _, _, _).
rebutRestriction(_, _, _, C) :- restrict(C).

restrict([_, TopRule, _, _, [[TopRule], _, _]]) :- TopRule \== none.
