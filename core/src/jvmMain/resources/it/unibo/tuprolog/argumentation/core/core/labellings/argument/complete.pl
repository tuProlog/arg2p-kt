argumentLabelling :-
    cache_retract(complete(_,_,_)),
    grounded::argumentLabelling,
    cache_dynamic_active(Branch),
    completeLabelling(Branch).

completeLabelling(_) :-
    admissibleSet,
    utils::recoverArgumentLabelling(In, Out, Und),
    \+ cache_check(complete(In, Out, Und)),
    cache_assert(complete(In, Out, Und)).
completeLabelling(Branch) :-
    cache_dynamic_check(und(X)),
    cache_dynamic_branch(Branch, NewBranch),
    findall(Y, (cache_dynamic_check(und(Y)), Y \= X), Arguments),
    cache_dynamic_retract(und(_)),
    cache_dynamic_assert(in(X)),
    grounded::groundedLabelling(Arguments),
    completeLabelling(NewBranch).

% If there is an attacker to an In argument, then should exist also a In argument attacking the attacker

admissibleSet :-
    \+ (cache_dynamic_check(in(H)), \+ admissible(H)).
admissible(H) :-
    \+ (
        cache_check(attack(_, Attacker, H, _)),
        \+ (
            cache_check(attack(_, Defendant, Attacker, _)),
            cache_dynamic_check(in(Defendant))
        )
    ).
