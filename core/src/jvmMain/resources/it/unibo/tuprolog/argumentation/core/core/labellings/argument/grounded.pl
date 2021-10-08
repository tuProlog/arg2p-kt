argumentLabelling :-
    cache_dynamic_branch(0, _),
    findall(X, cache_check(argument(X)), Arguments),
    groundedLabelling(Arguments).

groundedLabelling(UND) :-
    member(A, UND), 
    allAttacksOUT(A), !,
    utils::subtract(UND, [A], NewUND),
    cache_dynamic_assert(in(A)),
    groundedLabelling(NewUND).
groundedLabelling(UND) :-
    member(A, UND),
    oneAttackIN(A), !,
    utils::subtract(UND, [A], NewUND),
    cache_dynamic_assert(out(A)),
    groundedLabelling(NewUND).
groundedLabelling(Args) :- findall(_, (member(A, Args), cache_dynamic_assert(und(A))), _).

% If an attack exists, it should come from an OUT argument

allAttacksOUT(A) :-
    \+ ( cache_check(attack(_, B, A, _)), \+ (cache_dynamic_check(out(B)))).

% Find an attack, if exists, from an IN argument, then ends

oneAttackIN(A) :-
    cache_check(attack(_, B, A, _)),
    cache_dynamic_check(in(B)), !.

% If A attacks an IN argument, then A is OUT

oneAttackIN(A) :-
    cache_check(attack(_, A, B, _)),
    cache_dynamic_check(in(B)), !.
