modifyArgumentationGraph :-
    findall(_, (
        context_check(attack(T, A, B, C)),
        checkStandardPreference(T, A, B, C)
    ), _).

checkStandardPreference(T, A, B, C) :-
    \+ standardPreferences(T, A, B, C),
    standard_af::removeAttack(T, A, B, C).

% The attack types preferences do not apply to. Everything else is weighed against the preference order,
% including the attack types introduced by other extensions: an unknown attack type is compared rather than
% dropped, so that the result does not depend on the order in which the extensions run.
standardPreferences(contrary_rebut, _, _, _).
standardPreferences(contrary_undermine, _, _, _).
standardPreferences(undercut, _, _, _).
standardPreferences(_, A, _, C) :- \+ superiority::superiorArgument(C, A).
