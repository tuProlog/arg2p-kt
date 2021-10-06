modifyArgumentationGraph(Rules, [Arguments, Attacks, Supports], [Arguments, NewAttacks, Supports]) :-
    superiority::retractPreferenceCache,
    findall(sup(X, Y), member(sup(X, Y), Rules), Pref),
    superiority::setupPreferences(Pref),
    checkStandardPreferences(Attacks, NewAttacks).

checkStandardPreferences([], []).
checkStandardPreferences([Attack|Attacks], NewAttacks) :-
    checkStandardPreferences(Attacks, TempAttacks),
	checkStandardPreference(Attack, R),
	utils::appendLists([R, TempAttacks], NewAttacks).

checkStandardPreference((T, A, B, C), [(T, A, B, C)]) :-
    standardPreferences(T, A, B, C), !.
checkStandardPreference((T, A, B, C), []).

standardPreferences(rebut, A, _, C) :- \+ superiority::superiorArgument(C, A).
standardPreferences(undermine, A, _, C) :- \+ superiority::superiorArgument(C, A).
standardPreferences(contrary_rebut, _, _, _).
standardPreferences(contrary_undermine, _, _, _).
standardPreferences(undercut, _, _, _).
