modifyArgumentationGraph(defeasibleAllPref, [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
    retractall(sup(_,_)),
    convertAttacks(Attacks, [NewArguments, NewAttacks, NewSupports]), !,
    appendLists([Arguments, NewArguments], Args),
    buildPrefAttacks(Args, NewAttacks, ResArguments, ResAttacks, ResSupports), !,
    appendLists([Args, ResArguments], UnionArguments),
    appendLists([NewAttacks, ResAttacks], UnionAttacks),
    appendLists([Supports, NewSupports, ResSupports], UnionSupports), !.

buildPrefAttacks(Arguments, Attacks, ResArguments, ResAttacks, ResSupports) :-
    findall(_, (member([_, _, [sup(RuleOne, RuleTwo)]], Arguments), asserta(sup(RuleOne, RuleTwo))), _),
    findPrefAttack(Arguments, Attacks, [], [], [], ResArguments, ResAttacks, ResSupports), !,
    retractall(sup(_, _)).

findPrefAttack(Arguments, Attacks, TempArguments, TempAttacks, TempSupports, ResArguments, ResAttacks, ResSupports) :-
    member([IdB, attack, attack(T, A, B, C)], Arguments),
    invalid(T, A, B, C, SupSet),
    recoverSuperiorityArgument(SupSet, Arguments, Attacks, Arg, Supps, Atts),
    Attack = (pref, Arg, [IdB, attack, attack(T, A, B, C)]),
    \+ member(Attack, TempAttacks),
    asserta(attack(pref, Arg, [IdB, attack, attack(T, A, B)])),
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

invalid(rebut, A, B, C, SupSet) :- superiorArgument(B, A, C, SupSet), !.
invalid(undermine, A, B, C, SupSet) :- superiorArgument(B, A, C, SupSet), !.
