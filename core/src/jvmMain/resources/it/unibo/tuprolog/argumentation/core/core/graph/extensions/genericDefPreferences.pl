modifyArgumentationGraph :-
    defeasiblePref::assertAllSup,
    findall((T, A, B, C), context_check(attack(T, A, B, C)), Attacks),
    defeasiblePref::filterSupRelatedAttacks(Attacks, InvalidAttacks),
    defeasiblePref::convertAttacks(InvalidAttacks),
    buildPrefAttacks,
    standard_af::buildTransitiveAttacks.


buildPrefAttacks :-
    context_check(argument([IdB, attack, [attack(T, A, B, C)], G, I])),
    defeasiblePref::invalid(T, A, B, C, SupSet),
    createSuperiorityArgument(SupSet, Arg),
    % write(Arg), nl,
    % Attack = attack(pref, Arg, [IdB, attack, [attack(T, A, B, C)], G, I], [IdB, attack, [attack(T, A, B, C)], G, I]),
    % \+ context_check(Attack),
    % context_assert(Attack),
    standard_af::saveAttack(pref, Arg, [IdB, attack, [attack(T, A, B, C)], G, I], [IdB, attack, [attack(T, A, B, C)], G, I]),
    fail.
buildPrefAttacks.


createSuperiorityArgument(SupSet, Argument) :-
    findall([Id, TR, [X], G, I], (
        member(X, SupSet),
        context_check(argument([Id, TR, [X], G, I]))
    ), SupportArguments),
    mergeIds(SupportArguments, MergedId),
    Argument = [MergedId, pref, [preference(SupSet)], [], [[pref], MergedId, []]],
    % \+ context_check(argument(Argument)),
    % context_assert(argument(Argument)),
    standard_af::saveArgument(a, [preference(SupSet)], Argument),
    findall(_, (
        member(A, SupportArguments),
        standard_af::saveSupport(A, Argument)
    ), _).

mergeIds([], [pref]).
mergeIds(Arguments, [pref|MergedId]) :-
    findall(Id, member([Id, _, _, _, _], Arguments), Res),
    utils::appendLists(Res, MergedId).
