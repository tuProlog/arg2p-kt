modifyArgumentationGraph(defeasiblePref, [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
    retractall(sup(_,_)),
    retractPreferenceCache,
    assertAllSup(Arguments),
    once(filterSupRelatedAttacks(Attacks, ValidAttacks, InvalidAttacks)),
    convertAttacks(InvalidAttacks, [NewArguments, NewAttacks, NewSupports]),
    buildPrefAttacks(Arguments, NewArguments, PrefAttacks),
    retractall(sup(_,_)),
    retractPreferenceCache,
    appendLists([Arguments, NewArguments], UnionArguments),
    appendLists([ValidAttacks, NewAttacks, PrefAttacks], UnionAttacks),
    appendLists([Supports, NewSupports], UnionSupports), !.

assertAllSup(Arguments) :-
    findall(_,
        (
            member([_, _, [sup(RuleOne, RuleTwo)]], Arguments),
            \+ sup(RuleOne, RuleTwo),
            asserta(sup(RuleOne, RuleTwo))
        ),
    _).

filterSupRelatedAttacks([], [], []).
filterSupRelatedAttacks([(T, A, B)|Attacks], Valid, [(T, A, B)|Invalid]) :-
    attack(T, A, B, C),
    invalid(T, A, B, C, _),
    filterSupRelatedAttacks(Attacks, Valid, Invalid).
filterSupRelatedAttacks([(T, A, B)|Attacks], [(T, A, B)|Valid], Invalid) :-
    attack(T, A, B, C),
    \+ invalid(T, A, B, C, _),
    filterSupRelatedAttacks(Attacks, Valid, Invalid).

/*
*   Translates the attack relations identified during the building phase.
*   Attack from A to B -> Argument [[], attack, Attack]
*                         support(A, Argument)
*                         attack(Argument, B)
*   If an Argument A built in this way attacks the argument B, and this one also attacks a third argument C
*   through the argument B1 we have to consider an attack from A to B1 (transitive attack)
*/
convertAttacks(Attacks, [NewArguments, NewAttacks, NewSupports]) :-
    simpleConversion(Attacks, NewArguments, TempAttacks, NewSupports),
    transitiveConversion(TempAttacks, NewSupports, [], TransAttacks), !,
    appendLists([TempAttacks, TransAttacks], NewAttacks).

simpleConversion([], [], [], []) :- !.
simpleConversion(List, TmpArgs, [RAttack|TmpAtts], TmpSupps) :-
    member((T, A, B), List),
    attack(T, A, B, C),
    C \= B, !,
    subtract(List, [(T, A, B)], Tail),
    simpleConversion(Tail, TmpArgs, TmpAtts, TmpSupps),
    generateTransRebArg((T, A, B, C), RAttack).
simpleConversion(List, [RArgument|TmpArgs], [RAttack|TmpAtts], [RSupport|TmpSupps]) :-
    member((T, A, B), List),
    attack(T, A, B, B), !,
    subtract(List, [(T, A, B)], Tail),
    simpleConversion(Tail, TmpArgs, TmpAtts, TmpSupps),
    generateDirectRebArg((T, A, B), RArgument, RSupport, RAttack).

generateTransRebArg((T, A, B, C), (T, [[Id], attack, attack(T, A, C, C)], B)) :-
    argument([[Id], attack, attack(T, A, C, C)]),
    asserta(attack(T, [[Id], attack, attack(T, A, C, C)], B)),
    asserta(attack(T, [[Id], attack, attack(T, A, C, C)], B, C)),
    retractall(attack(T, A, B)),
    retractall(attack(T, A, B, C)).

generateDirectRebArg((T, A, B), RArgument, (A, RArgument), (T, RArgument, B)) :-
    generateId(A, B, Id),
    RArgument = [[Id], attack, attack(T, A, B, B)],
    \+ argument(RArgument),
    asserta(argument(RArgument)),
    asserta(support(A, RArgument)),
    asserta(attack(T, RArgument, B)),
    asserta(attack(T, RArgument, B, B)),
    retractall(attack(T, A, B)),
    retractall(attack(T, A, B, B)).

generateId([IdA, _, _], [IdB, _, _], Res) :-
    concate(IdA, A),
    concate(IdB, B),
    concate([A, B], Res).

concate([],'').
concate([X|Tail], Res) :-
	concate(Tail, IntermediateRes),
   	atom_concat(X, IntermediateRes, Res).

transitiveConversion(Attacks, Supports, TempAttacks, ResAttacks) :-
    member((T, A, B), Attacks),
    member((B, C), Supports),
    ResAttack = (T, A, C),
    \+ attack(T, A, C, D),
    attack(T, A, B, D),
    asserta(attack(T, A, C)),
    asserta(attack(T, A, C, D)),
    transitiveConversion(Attacks, Supports, [ResAttack|TempAttacks], ResAttacks).

transitiveConversion(_, _, TempAttacks, TempAttacks).

/*
*   Computes the pref attack. If an Argument A has a conclusion in the form sup(a, b), we verify if
*   the attacks involving arguments built on the rules a or b are compatible with this preference.
*   If there are some contradictions we add an attack from the argument A towards the incompatible attack
*/
buildPrefAttacks(Arguments, AttackArguments, PrefAttacks) :-
    findPrefAttack(Arguments, AttackArguments, [], PrefAttacks), !.

findPrefAttack(Arguments, AttackArguments, TempAttacks, ResAttacks) :-
    member([IdB, attack, attack(T, A, B, C)], AttackArguments),
    invalid(T, A, B, C, SupSet),
    member(X, SupSet),
    member([IdA, TRA, [X]], Arguments),
    Attack = (pref, [IdA, TRA, [X]], [IdB, attack, attack(T, A, B, C)]),
    \+ member(Attack, TempAttacks),
    asserta(attack(pref, [IdA, TRA, [X]], [IdB, attack, attack(T, A, B)])),
    asserta(attack(pref, [IdA, TRA, [X]], [IdB, attack, attack(T, A, B, C)], [IdB, attack, attack(T, A, B, C)])),
    findPrefAttack(Arguments, AttackArguments, [Attack|TempAttacks], ResAttacks).

findPrefAttack(_, _, TempAttacks, TempAttacks).

/*
*   Specification of a new constraint in the contrary function
*/
conflict([sup(X, Y)],  [sup(Y, X)]).

invalid(rebut, A, B, C, SupSet) :- superiorArgument(B, A, C, SupSet), !.
invalid(undermine, A, B, C, SupSet) :- superiorArgument(B, A, C, SupSet), !.
