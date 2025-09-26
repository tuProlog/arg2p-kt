argumentLabelling :-
    cache_retract(complete(_,_,_,_)),
    grounded:::argumentLabelling,
    context_active(Branch),
    write("merda"),nl,
    completeLabelling(Branch).

completeLabelling(Branch) :-
    admissibleSet,
    utils::recoverArgumentLabellingId(In, Out, Und),
    \+ cache_check(complete(In, Out, Und, _)),
    cache_assert(complete(In, Out, Und, Branch)).
completeLabelling(Branch) :-
    context_check(undId(IdX)),
    context_check(clause(arg(IdX), argument(X))),
    context_branch(Branch, NewBranch),
    context_retract(und(_)),
    context_retract(undId(_)),
    context_assert(in(X)),
    context_assert(inId(IdX)),
    grounded::argumentLabelling,
    completeLabelling(NewBranch).

% If there is an attacker to an In argument, then should exist also a In argument attacking the attacker

admissibleSet :-
    \+ (context_check(inId(H)), \+ admissible(H)).
admissible(H) :-
    \+ (
        context_check(clause(att(Attacker, H), _)),
        \+ (
            context_check(clause(att(Defendant, Attacker), _)),
            context_check(inId(Defendant))
        )
    ).
