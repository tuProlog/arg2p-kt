argumentLabelling :-
    cache_retract(complete(_,_,_)),
    grounded:::argumentLabelling,
    context_active(Branch),
    completeLabelling(Branch).

completeLabelling(_) :-
    admissibleSet,
    utils::recoverArgumentLabelling(In, Out, Und),
    \+ cache_check(complete(In, Out, Und)),
    cache_assert(complete(In, Out, Und)).
completeLabelling(Branch) :-
    context_check(und(X)),
    context_branch(Branch, NewBranch),
    findall(Y, (context_check(und(Y)), Y \= X), Arguments),
    context_retract(und(_)),
    context_assert(in(X)),
    grounded::groundedLabelling(Arguments),
    completeLabelling(NewBranch).

% If there is an attacker to an In argument, then should exist also a In argument attacking the attacker

admissibleSet :-
    \+ (context_check(in(H)), \+ admissible(H)).
admissible(H) :-
    \+ (
        context_check(attack(_, Attacker, H, _)),
        \+ (
            context_check(attack(_, Defendant, Attacker, _)),
            context_check(in(Defendant))
        )
    ).
