argumentLabelling(_, [Arguments, Attacks, Supports], [In, Out, Und]) :-
    cache_retract(complete(_,_,_)),
    grounded::argumentLabelling([Arguments, Attacks, Supports], [GroundedIn, GroundedOut, GroundedUnd]),
    completeLabelling(Arguments, Attacks, GroundedIn, GroundedOut, GroundedUnd, In, Out, Und).

completeLabelling(_, Attacks, In, Out, Und, In, Out, Und) :-
    admissible(In, In, Attacks),
    \+ cache_check(complete(In, Out, Und)),
    cache_assert(complete(In, Out, Und)).
completeLabelling(Arguments, Attacks, In, Out, Und, ResultIn, ResultOut, ResultUnd) :-
    member(X, Und),
    utils::subtract(Und, [X], NewUnd),
    grounded::groundedLabelling(Attacks, [X|In], Out, NewUnd, TempIn, TempOut, TempUnd),
    completeLabelling(Arguments, Attacks, TempIn, TempOut, TempUnd, ResultIn, ResultOut, ResultUnd).

% If there is an attacker to an In argument, then should exist also a In argument attacking the attacker

admissible(_, [], _).
admissible(In, [H|T], Attacks) :-
    \+ (
        member((_, Attacker, H), Attacks),
        \+ (
            member((_, Defendant, Attacker, _), Attacks),
            member(Defendant, In)
        )
    ),
    admissible(In, T, Attacks).
