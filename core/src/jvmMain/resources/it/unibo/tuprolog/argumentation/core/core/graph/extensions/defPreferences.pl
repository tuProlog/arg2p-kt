modifyArgumentationGraph(defeasiblePref, [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
    retractall(sup(_,_)),
    convertAttacks(Attacks, [NewArguments, NewAttacks, NewSupports]),
    buildPrefAttacks(Arguments, NewArguments, PrefAttacks),
    appendLists([Arguments, NewArguments], UnionArguments),
    appendLists([NewAttacks, PrefAttacks], UnionAttacks),
    appendLists([Supports, NewSupports], UnionSupports), !.

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

simpleConversion([], [], [], []).
simpleConversion([(T, A, B)|Tail], [RArgument|TmpArgs], [RAttack|TmpAtts], [RSupport|TmpSupps]) :-
    simpleConversion(Tail, TmpArgs, TmpAtts, TmpSupps),
    generateId(A, B, Id),
    attack(T, A, B, C),
    RArgument = [[Id], attack, attack(T, A, B, C)],
    RSupport = (A, RArgument),
    RAttack = (T, RArgument, B),
    asserta(argument(RArgument)),
    asserta(support(A, RArgument)),
    asserta(attack(T, RArgument, B)),
    asserta(attack(T, RArgument, B, C)),
    retractall(attack(T, A, B)),
    retractall(attack(T, A, B, C)).

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
    \+ member(ResAttack, TempAttacks),
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
    member([IdA, TRA, [sup(RuleOne, RuleTwo)]], Arguments),
    member([IdB, attack, attack(T, A, B, C)], AttackArguments),
    eligible(RuleOne, RuleTwo, A, C),
    asserta(sup(RuleOne, RuleTwo)),
    invalid(T, A, B, C),
    retractall(sup(RuleOne, RuleTwo)),
    Attack = (pref, [IdA, TRA, [sup(RuleOne, RuleTwo)]], [IdB, attack, attack(T, A, B, C)]),
    \+ member(Attack, TempAttacks),
    asserta(attack(pref, [IdA, TRA, [sup(RuleOne, RuleTwo)]], [IdB, attack, attack(T, A, B)])),
    asserta(attack(pref, [IdA, TRA, [sup(RuleOne, RuleTwo)]], [IdB, attack, attack(T, A, B, C)], [IdB, attack, attack(T, A, B, C)])),
    findPrefAttack(Arguments, AttackArguments, [Attack|TempAttacks], ResAttacks).

findPrefAttack(_, _, TempAttacks, TempAttacks).

eligible(RuleOne, RuleTwo, [R1, _, _], [R2, _, _]) :-
    (member(RuleOne, R1);member(RuleTwo, R1);member(RuleOne, R2);member(RuleTwo, R2)), !.

invalid(rebut, A, B, C) :- superiorArgument(B, A, C), !.
invalid(undermine, A, B, C) :- superiorArgument(B, A, C), !.

/*
*   Specification of a new constraint in the contrary function
*/
conflict([sup(X, Y)],  [sup(Y, X)]).
