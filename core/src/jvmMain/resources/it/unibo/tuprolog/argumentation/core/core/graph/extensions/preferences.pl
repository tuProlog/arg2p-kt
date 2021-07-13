modifyArgumentationGraph(standardPref, [Arguments, Attacks, Supports], [Arguments, NewAttacks, Supports]) :-
    supRules,
    retractPreferenceCache,
    checkStandardPreferences(Attacks, NewAttacks),
    retractPreferenceCache, !.

supRules :-
    retractall(superiority(_, _)),
    findall(_, (sup(X, Y), \+ superiority(X, Y), asserta(superiority(X, Y))), _).

checkStandardPreferences([], []).
checkStandardPreferences([Attack|Attacks], NewAttacks) :-
    checkStandardPreferences(Attacks, TempAttacks),
	once(checkStandardPreference(Attack, R)),
	appendLists([R, TempAttacks], NewAttacks).

checkStandardPreference((T, A, B), [(T, A, B)]) :-
    attack(T, A, B, C),
    standardPreferences(T, A, B, C), !.
checkStandardPreference((T, A, B), []) :-
    attack(T, A, B, C),
    retractall(attack(T, A, B, C)),
    retractall(attack(T, A, B)).

standardPreferences(rebut, A, B, C) :- \+ superiorArgument(B, A, C).
standardPreferences(undermine, A, B, C) :- \+ superiorArgument(B, A, C).
standardPreferences(contrary_rebut, _, _, _).
standardPreferences(contrary_undermine, _, _, _).
standardPreferences(undercut, _, _, _).
