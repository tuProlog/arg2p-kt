modifyArgumentationGraph :-
    findall(_, (
        context_check(attack(T, A, B, C)),
        checkStandardPreference(T, A, B, C)
    ), _).

% Negation is written as cut and fail: \+ would go through call/1, which visits the whole goal (and with it
% the two arguments) before running it
checkStandardPreference(T, A, B, C) :- standardPreferences(T, A, C), !, fail.
checkStandardPreference(T, A, B, C) :- standard_af::removeAttack(T, A, B, C).

% The attack types preferences do not apply to. Everything else is weighed against the preference order,
% including the attack types introduced by other extensions: an unknown attack type is compared rather than
% dropped, so that the result does not depend on the order in which the extensions run.
standardPreferences(contrary_rebut, _, _) :- !.
standardPreferences(contrary_undermine, _, _) :- !.
standardPreferences(undercut, _, _) :- !.
standardPreferences(_, A, C) :- superiority::superiorArgument(C, A), !, fail.
standardPreferences(_, _, _).
