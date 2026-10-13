% Attacks are visited by argument identifiers, and arguments are compared by their rules and premises only
% (argInfo): the whole arguments are only retrieved to remove an attack
modifyArgumentationGraph :-
    findall(_, (
        context_check(attackId(T, IdA, IdB, IdC)),
        checkStandardPreference(T, IdA, IdB, IdC)
    ), _).

% Negation is written as cut and fail: \+ would go through call/1, which visits the whole goal before running it
checkStandardPreference(T, IdA, _, IdC) :- standardPreferences(T, IdA, IdC), !, fail.
checkStandardPreference(T, IdA, IdB, IdC) :-
    argById(IdA, A),
    argById(IdB, B),
    argById(IdC, C),
    standard_af::removeAttack(T, A, B, C).

argById(Id, A) :- context_check(clause(arg(Id), argument(A))), !.
% Attacks buffered while answering queries have none in place of the attacked argument
argById(Id, none) :- context_id(argument(none), Id).

% The attack types preferences do not apply to. Everything else is weighed against the preference order,
% including the attack types introduced by other extensions: an unknown attack type is compared rather than
% dropped, so that the result does not depend on the order in which the extensions run.
standardPreferences(contrary_rebut, _, _) :- !.
standardPreferences(contrary_undermine, _, _) :- !.
standardPreferences(undercut, _, _) :- !.
standardPreferences(_, IdA, IdC) :-
    context_check(argInfo(IdA, InfoA)),
    context_check(argInfo(IdC, InfoC)),
    superiority::superiorArgument([_, _, _, _, InfoC], [_, _, _, _, InfoA]), !,
    fail.
standardPreferences(_, _, _).
