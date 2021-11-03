argumentLabelling :-
    findall(X, context_check(argument(X)), Arguments),
    groundedLabelling(Arguments).

groundedLabelling(UND) :-
    member(A, UND), 
    allAttacksOUT(A), !,
    utils::subtract(UND, [A], NewUND),
    context_assert(in(A)),
    groundedLabelling(NewUND).
groundedLabelling(UND) :-
    member(A, UND),
    oneAttackIN(A), !,
    utils::subtract(UND, [A], NewUND),
    context_assert(out(A)),
    groundedLabelling(NewUND).
groundedLabelling(Args) :- findall(_, (member(A, Args), context_assert(und(A))), _).

% If an attack exists, it should come from an OUT argument

allAttacksOUT(A) :-
    \+ ( context_check(attack(_, B, A, _)), \+ (context_check(out(B)))).

% Find an attack, if exists, from an IN argument, then ends

oneAttackIN(A) :-
    context_check(attack(_, B, A, _)),
    context_check(in(B)), !.

% If A attacks an IN argument, then A is OUT

oneAttackIN(A) :-
    context_check(attack(_, A, B, _)),
    context_check(in(B)), !.
