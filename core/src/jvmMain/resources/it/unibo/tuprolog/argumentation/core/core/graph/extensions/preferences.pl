modifyArgumentationGraph(Rules, [Arguments, Attacks, Supports], [Arguments, NewAttacks, Supports]) :-
    superiority::retractPreferenceCache,
    checkStandardPreferences(Rules, Attacks, NewAttacks),
    superiority::retractPreferenceCache.

checkStandardPreferences(_, [], []).
checkStandardPreferences(Rules, [Attack|Attacks], NewAttacks) :-
    checkStandardPreferences(Rules, Attacks, TempAttacks),
	checkStandardPreference(Rules, Attack, R),
	utils::appendLists([R, TempAttacks], NewAttacks).

checkStandardPreference(Rules, (T, A, B, C), [(T, A, B, C)]) :-
    standardPreferences(Rules, T, A, B, C), !.
checkStandardPreference(_, (T, A, B, C), []).

standardPreferences(Rules, rebut, A, B, C) :- \+ superiority::superiorArgument(Rules, B, A, C).
standardPreferences(Rules, undermine, A, B, C) :- \+ superiority::superiorArgument(Rules, B, A, C).
standardPreferences(_, contrary_rebut, _, _, _).
standardPreferences(_, contrary_undermine, _, _, _).
standardPreferences(_, undercut, _, _, _).
