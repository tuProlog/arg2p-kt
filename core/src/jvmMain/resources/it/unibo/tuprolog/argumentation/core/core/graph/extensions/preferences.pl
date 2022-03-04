modifyArgumentationGraph :-
    findall(_, (
        context_check(attack(T, A, B, C)),
        checkStandardPreference(T, A, B, C)
    ), _).

checkStandardPreference(T, A, B, C) :-
    \+ standardPreferences(T, A, B, C),
    context_retract(attack(T, A, B, C)),
    hashed_attack(attack(T, A, B, C), Y),
    context_retract(Y).

standardPreferences(rebut, A, _, C) :- \+ superiority::superiorArgument(C, A).
standardPreferences(undermine, A, _, C) :- \+ superiority::superiorArgument(C, A).
standardPreferences(contrary_rebut, _, _, _).
standardPreferences(contrary_undermine, _, _, _).
standardPreferences(undercut, _, _, _).


hashed_attack(attack(T, A, B, C), att(IdA, IdB) :- attack(T, A, B, C)) :-
    utils::hash(argument(A), IdA),
    utils::hash(argument(B), IdB).
