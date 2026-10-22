% Lists Utils

writeList([]) :- nl.
writeList([X|Others]) :-
	write(X),
	writeList(Others).


writeListNl([]).
writeListNl([X|Others]) :-
	write(X),nl,
	writeListNl(Others).



% sortDistinct/2, deduplicate/2 and sort/2 are native primitives (see Utils.kt)

% subtract/3 is a native primitive (see Utils.kt)


isEmptyList([]).


% Misc utils

% search(+F, +L, -S): S is each term F(...) of arity 1 to L that holds
search(F, L, S) :-
   between(1, L, N),
   functor(S, F, N),
   call(S).



recoverGraph(Args, Attacks, Supports) :-
        findall(X, context_check(argument(X)), TempArgs),
        findall((T, A, B, C), context_check(attack(T, A, B, C)), TempAttacks),
        findall((A, B), context_check(support(A, B)), TempSupports),
        utils::sort(TempArgs, Args),
        utils::sort(TempAttacks, Attacks),
        utils::sort(TempSupports, Supports).

recoverArgumentLabelling(ArgsIn, ArgsOut, ArgsUnd) :-
        findall(X, context_check(in(X)), TempArgsIn),
        findall(X, context_check(out(X)), TempArgsOut),
        findall(X, context_check(und(X)), TempArgsUnd),
        utils::sort(TempArgsIn, ArgsIn),
        utils::sort(TempArgsOut, ArgsOut),
        utils::sort(TempArgsUnd, ArgsUnd).

recoverStatementLabelling(In, Out, Und) :-
        findall(X, context_check(statIn(X)), TempIn),
        findall(X, context_check(statOut(X)), TempOut),
        findall(X, context_check(statUnd(X)), TempUnd),
        utils::sort(TempIn, In),
        utils::sort(TempOut, Out),
        utils::sort(TempUnd, Und).

recoverArgumentLabellingAbstract(ArgsIn, ArgsOut, ArgsUnd) :-
        findall(X, context_check(in([_, _, [X], _, _])), TempArgsIn),
        findall(X, context_check(out([_, _, [X], _, _])), TempArgsOut),
        findall(X, context_check(und([_, _, [X], _, _])), TempArgsUnd),
        utils::sort(TempArgsIn, ArgsIn),
        utils::sort(TempArgsOut, ArgsOut),
        utils::sort(TempArgsUnd, ArgsUnd).
