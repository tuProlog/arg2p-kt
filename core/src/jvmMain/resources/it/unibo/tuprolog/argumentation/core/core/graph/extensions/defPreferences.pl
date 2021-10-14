modifyArgumentationGraph :-
    assertAllSup,
    findall((T, A, B, C), context_check(attack(T, A, B, C)), Attacks),
    filterSupRelatedAttacks(Attacks, InvalidAttacks),
    convertAttacks(InvalidAttacks),
    buildPrefAttacks.

% Add the defeasible rules in the theory to the work context

assertAllSup :-
    context_retract(sup(_, _)),
    findall(_,
        (
            context_check(argument([_, _, [sup(RuleOne, RuleTwo)], _, _])),
            \+ context_check(sup(RuleOne, RuleTwo)),
            context_assert(sup(RuleOne, RuleTwo))
        ),
    _).

filterSupRelatedAttacks([], []).
filterSupRelatedAttacks([(T, A, B, C)|Attacks], [(T, A, B, C)|Invalid]) :-
    invalid(T, A, B, C, _),
    filterSupRelatedAttacks(Attacks, Invalid), !.
filterSupRelatedAttacks([_|Attacks], Invalid) :-
    filterSupRelatedAttacks(Attacks, Invalid).


invalid(rebut, A, B, C, SupSet) :- superiority::superiorArgument(C, A, SupSet), !.
invalid(undermine, A, B, C, SupSet) :- superiority::superiorArgument(C, A, SupSet), !.


/*
*   Translates the attack relations identified during the building phase.
*   Attack from A to B -> Argument [[], attack, Attack]
*                         support(A, Argument)
*                         attack(Argument, B)
*   If an Argument A built in this way attacks the argument B, and this one also attacks a third argument C
*   through the argument B1 we have to consider an attack from A to B1 (transitive attack)
*/

convertAttacks(Attacks) :-
    conversion(Attacks),
    standard_af::buildTransitiveAttacks.

conversion(List) :-
    member((T, A, B, B), List),
    generateDirectAttackArgument(T, A, B, B),
    fail.
conversion(List) :-
    member((T, A, B, C), List),
    C \= B,
    generateTransitiveAttackArgument(T, A, B, C),
    fail.
conversion(_).

generateTransitiveAttackArgument(T, A, B, C) :-
    context_check(argument([Id, attack, [attack(T, A, C, C)], G, I])),
    context_assert(attack(T, [Id, attack, [attack(T, A, C, C)], G, I], B, C)),
    context_retract(attack(T, A, B, C)).

generateDirectAttackArgument(T, A, B, B) :-
    RArgument = [[attack], attack, [attack(T, A, B, B)], [], [[attack], [attack], []]],
    \+ context_check(argument(RArgument)),
    context_assert(argument(RArgument)),
    context_assert(support(A, RArgument)),
    context_assert(attack(T, RArgument, B, B)),
    context_retract(attack(T, A, B, B)).


/*
*   Computes the pref attack. If an Argument A has a conclusion in the form sup(a, b), we verify if
*   the attacks involving arguments built on the rules a or b are compatible with this preference.
*   If there are some contradictions we add an attack from the argument A towards the incompatible attack
*/

buildPrefAttacks :-
    context_check(argument([IdB, attack, [attack(T, A, B, C)], G, I])),
    invalid(T, A, B, C, SupSet),
    member(X, SupSet),
    context_check(argument([IdA, TRA, [X], GG, II])),
    Attack = attack(pref, [IdA, TRA, [X], GG, II], [IdB, attack, [attack(T, A, B, C)], G, I], [IdB, attack, [attack(T, A, B, C)], G, I]),
    \+ context_check(Attack),
    context_assert(Attack),
    fail.
buildPrefAttacks.
