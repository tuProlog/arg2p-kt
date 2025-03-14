modifyArgumentationGraph :-
    findall(_, (
        context_check(attack(T, A, B, C)),
        checkStandardPreference(T, A, B, C)
    ), _).

checkStandardPreference(T, A, B, C) :-
    \+ standardPreferences(T, A, B, C),
    standard_af::removeAttack(T, A, B, C).

standardPreferences(rebut, A, _, C) :- \+ superiority::superiorArgument(C, A).
standardPreferences(undermine, A, _, C) :- \+ superiority::superiorArgument(C, A).
standardPreferences(contrary_rebut, _, _, _).
standardPreferences(contrary_undermine, _, _, _).
standardPreferences(undercut, _, _, _).
