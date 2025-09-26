argumentLabelling :-
    groundedLabelling.

groundedLabelling :-
    findall(t, evaluateIn, I),
    findall(t, evaluateOut, O),
    finalize(I, O).

evaluateIn :-
    argument_to_evaluate(A, IdA),
    allAttacksOUT(IdA),
    context_assert(in(A)),
    context_assert(inId(IdA)).

evaluateOut :-
    argument_to_evaluate(A, IdA),
    oneAttackIN(IdA),
    context_assert(out(A)),
    context_assert(outId(IdA)).

finalize(I, O) :- I \= [], !, groundedLabelling.
finalize(I, O) :- O \= [], !, groundedLabelling.

finalize([], []) :-
    argument_to_evaluate(A, IdA),
    context_assert(und(A)),
    context_assert(undId(IdA)),
    fail.
finalize(_, _).

argument_to_evaluate(X, IdX) :-
    context_check(clause(arg(IdX), argument(X))),
    \+ context_check(outId(IdX)),
    \+ context_check(inId(IdX)).

% If an attack exists, it should come from an OUT argument

allAttacksOUT(A) :-
    \+ ( context_check(clause(att(B, A), _)), \+ (context_check(outId(B)))).

% Find an attack, if exists, from an IN argument, then ends

oneAttackIN(A) :-
    context_check(clause(att(B, A), _)),
    context_check(inId(B)), !.

% If A attacks an IN argument, then A is OUT

oneAttackIN(A) :-
    context_check(clause(att(A, B), _)),
    context_check(inId(B)), !.
