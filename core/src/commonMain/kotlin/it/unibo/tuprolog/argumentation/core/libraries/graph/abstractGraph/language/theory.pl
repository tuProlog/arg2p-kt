%! statedArguments(-Arguments)
%  The arguments the theory states or derives, in either notation (`a.`, arg(a)).
statedArguments(Arguments) :-
    findall(A, user::arg(A), Stated),
    findall(A, (theoryClause(A, true), atom(A)), Atoms),
    append(Stated, Atoms, Arguments).

%! statedAttacks(-Attacks)
%  The attacks the theory states or derives, in either notation (`a -> b.`, `a <-> b.`, att(a, b)), as att(A, B) terms.
statedAttacks(Attacks) :-
    findall(att(A, B), user::att(A, B), Stated),
    findall(att(A, B), (theoryClause((A -> B), Body), user::call(Body)), Forward),
    findall(att(A, B), (theoryClause((X <-> Y), Body), user::call(Body), (A = X, B = Y ; A = Y, B = X)), Mutual),
    utils::appendLists([Stated, Forward, Mutual], Attacks).
