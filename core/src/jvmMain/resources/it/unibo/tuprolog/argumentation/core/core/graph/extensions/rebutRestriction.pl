modifyArgumentationGraph(rebutRestriction, Originals, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    checkRestrictions(Originals, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]), !.

checkRestrictions([], [Arguments, Attacks, Supports], [Arguments, Attacks, Supports]).
checkRestrictions([A|Attacks], [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    checkRestrictions(Attacks, [Arguments, Attacks, Supports], [TempArguments, TempAttacks, TempSupports]),
    checkRestriction(A, [TempArguments, TempAttacks, TempSupports], [NewArguments, NewAttacks, NewSupports]).

checkRestriction(A, [Arguments, Attacks, Supports], [[Conflict|NewArguments], [(resrebut, Conflict, [Id, TopRule, Conc])|NewAttacks], NewSupports]) :-
    restrict(A),
    convertAttack(A, [Arguments, Attacks, Supports], [Id, TopRule, Conc], [NewArguments, NewAttacks, NewSupports]),
    Conflict = [[artRebRes|Id], artRebRes, [restrict(A)]],
    asserta(argument(Conflict)),
    asserta(attack(resrebut, Conflict, [Id, TopRule, Conc])),
    asserta(attack(resrebut, Conflict, [Id, TopRule, Conc], [Id, TopRule, Conc])).
checkRestriction(A, [Arguments, Attacks, Supports], [Arguments, Attacks, Supports]) :-
    \+ restrict(A).

restrict((rebut, _, _, [_, TopRule, _ ])) :- TopRule \== none, strict(TopRule).
restrict((undermine, _, _, [[Premise], none, _ ])) :- strict(Premise).
