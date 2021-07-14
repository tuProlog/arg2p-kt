modifyArgumentationGraph(defeasibleAllPref, [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
    retractPreferenceCache,
    assertAllSup(Arguments),
    once(filterSupRelatedAttacks(Attacks, ValidAttacks, InvalidAttacks)),
    convertAttacks(Attacks, InvalidAttacks, [NewArguments, NewAttacks, NewSupports]),
    appendLists([Arguments, NewArguments], Args),
    appendLists([ValidAttacks, NewAttacks], Atts),
    buildPrefAttacks(Args, Atts, ResArguments, ResAttacks, ResSupports), !,
    retractPreferenceCache,
    appendLists([Args, ResArguments], UnionArguments),
    appendLists([Atts, ResAttacks], UnionAttacks),
    appendLists([Supports, NewSupports, ResSupports], UnionSupports), !.

buildPrefAttacks(Arguments, Attacks, ResArguments, ResAttacks, ResSupports) :-
    findPrefAttack(Arguments, Attacks, [], [], [], ResArguments, ResAttacks, ResSupports), !.

findPrefAttack(Arguments, Attacks, TempArguments, TempAttacks, TempSupports, ResArguments, ResAttacks, ResSupports) :-
    member([IdB, attack, attack(T, A, B, C)], Arguments),
    invalid(T, A, B, C, SupSet),
    recoverSuperiorityArgument(SupSet, Arguments, Attacks, Arg, Supps, Atts),
    Attack = (pref, Arg, [IdB, attack, attack(T, A, B, C)]),
    \+ member(Attack, TempAttacks),
    asserta(attack(pref, Arg, [IdB, attack, attack(T, A, B, C)])),
    asserta(attack(pref, Arg, [IdB, attack, attack(T, A, B, C)], [IdB, attack, attack(T, A, B, C)])),
    appendLists([Supps, TempSupports], NewSupports),
    appendLists([Atts, TempAttacks], NewAttacks),
    findPrefAttack(Arguments, Attacks, [Arg|TempArguments], [Attack|NewAttacks], NewSupports, ResArguments, ResAttacks, ResSupports).

findPrefAttack(_, _, ResArguments, ResAttacks, ResSupports, ResArguments, ResAttacks, ResSupports).

recoverSuperiorityArgument(SupSet, Arguments, Attacks, Arg, Supps, Atts) :-
    findInterestedArguments(SupSet, Arguments, IntArguments),
    createArgument(IntArguments, Arg, Supps),
    liftAttacks(Arg, IntArguments, Attacks, Atts).

findInterestedArguments(SupSet, Arguments, IntArguments) :-
    findall([Id, TR, [X]], (member(X, SupSet), member([Id, TR, [X]], Arguments)), IntArguments).

createArgument(IntArguments, Arg, Supps) :-
    mergeIds(IntArguments, Id),
    Arg = [Id, pref, [mergedPreference]],
    findall((A, Arg),
        (
            member(A, IntArguments),
            \+ support(A, Arg),
            asserta(support(A, Arg))
        ),
        Supps
    ).

mergeIds([], [pref]).
mergeIds(Arguments, [pref|Id]) :-
    findall(Id, member([Id, _, _], Arguments), Res),
    appendLists(Res, Id).

liftAttacks(Arg, IntArguments, Attacks, Atts) :-
    findall((T, B, Arg),
        (
            member(A, IntArguments),
            member((T, B, A), Attacks),
            attack(T, B, A, C),
            \+ attack(T, B, Arg, C),
            asserta(attack(T, B, Arg, C)),
            asserta(attack(T, B, Arg))
        ),
        Atts
    ).
