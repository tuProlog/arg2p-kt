argumentLabelling([Arguments, Attacks, _], [SortedIn, SortedOut, SortedUnd]) :-
    groundedLabelling(Arguments, Attacks, [], [], Arguments, In, Out, Und),
    utils::sort(In, SortedIn),
    utils::sort(Out, SortedOut),
    utils::sort(Und, SortedUnd).

groundedLabelling(Arguments, Attacks, IN, OUT, UND, ResultIN, ResultOUT, ResultUND) :-
    member(A, UND), 
    allAttacksOUT(Attacks, A, OUT), !,
    utils::subtract(UND, [A], NewUND),
    groundedLabelling(Arguments, Attacks, [A|IN], OUT, NewUND, ResultIN, ResultOUT, ResultUND).
groundedLabelling(Arguments, Attacks, IN, OUT, UND, ResultIN, ResultOUT, ResultUND) :-
    member(A, UND), 
    oneAttackIN(Attacks, A, IN), !,
    utils::subtract(UND, [A], NewUND),
    groundedLabelling(Arguments, Attacks, IN, [A|OUT], NewUND, ResultIN, ResultOUT, ResultUND).
groundedLabelling(_, _, IN, OUT, UND, IN, OUT, UND).

% If an attack exists, it should come from an OUT argument

allAttacksOUT(Attacks, A, OUT) :-
    \+ ( member((_, B, A, _), Attacks), \+ (member(B, OUT))).

% Find an attack, if exists, from an IN argument, then ends

oneAttackIN(Attacks, A, IN) :-
    member((_, B, A, _), Attacks),
    member(B, IN), !.

% If A attacks an IN argument, then A is OUT

oneAttackIN(Attacks, A, IN) :-
    member((_, A, B, _), Attacks),
    member(B, IN), !.
