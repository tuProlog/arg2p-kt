argumentLabelling :-
    groundedLabelling.

groundedLabelling :-
    argument_to_evaluate(A, IdA),
    allAttacksOUT(IdA), !,
    context_assert(in(A)),
    context_assert(inId(IdA)),
    groundedLabelling.
groundedLabelling :-
    argument_to_evaluate(A, IdA),
    oneAttackIN(IdA), !,
    context_assert(out(A)),
    context_assert(outId(IdA)),
    groundedLabelling.
groundedLabelling :- finalize.

finalize :-
    argument_to_evaluate(A, _),
    context_assert(und(A)),
    fail.
finalize.

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
