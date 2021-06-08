modifyArgumentationGraph(defpreferences, Originals, [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
    convertAttacks(Attacks, [NewArguments, NewAttacks, NewSupports]),
%    buildPrefAttacks(Arguments, NewArguments, PrefAttacks),
    appendLists([Arguments, NewArguments], UnionArguments),
    appendLists([NewAttacks, []], UnionAttacks),
    appendLists([Supports, NewSupports], UnionSupports), !.


/*
*   Computes the pref attack. If an Argument A has a conclusion in the form sup(a, b), we verify if
*   the attacks involving arguments built on the rules a or b are compatible with this preference.
*   If there are some contradictions we add an attack from the argument A towards the incompatible attack
*/
buildPrefAttacks(Arguments, AttackArguments, PrefAttacks) :-
    findPrefAttack(Arguments, AttackArguments, [], PrefAttacks), !.

findPrefAttack(Arguments, AttackArguments, TempAttacks, ResAttacks) :-
    member([IdA, TRA, [sup(RuleOne, RuleTwo)]], Arguments),
    member([[], attack, attack(T, A, B)], AttackArguments),
    eligible(RuleOne, RuleTwo, A, B),
    asserta(sup(RuleOne, RuleTwo)),
    invalid(T, A, B),
    retractall(sup(RuleOne, RuleTwo)),
    Attack = (pref, [IdA, TRA, [sup(RuleOne, RuleTwo)]], [[], attack, attack(T, A, B)]),
    \+ member(Attack, TempAttacks),
    asserta(attack(pref, [IdA, TRA, [sup(RuleOne, RuleTwo)]], [[], attack, attack(T, A, B)])),
    findPrefAttack(Arguments, AttackArguments, [Attack|TempAttacks], ResAttacks).

findPrefAttack(_, _, TempAttacks, TempAttacks).

eligible(RuleOne, RuleTwo, [R1, _, _], [R2, _, _]) :-
    \+ sup(RuleOne, RuleOne),
    (member(RuleOne, R1);member(RuleTwo, R1);member(RuleOne, R2);member(RuleTwo, R2)), !.

invalid(rebut, A, B) :- superiorArgument(B, A), !.
invalid(undermine, A, B) :- superiorArgument(B, A), !.

/*
*   Specification of a new constraint in the contrary function
*/
conflict([sup(X, Y)],  [sup(Y, X)]).
