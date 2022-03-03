%==============================================================================
% BP LABELLING [ICAIL]
%==============================================================================

argumentLabelling :-
    reifyBurdenOfProofs,
    filterBpDefeats,
    findall(X, context_check(argument(X)), Arguments),
    bpLabelling(Arguments).

bpLabelling(Arguments) :-
    member(A, Arguments),
    grounded::allAttacksOUT(A), !,
    context_assert(in(A)),
    utils::subtract(Arguments, [A], NewArguments),
    bpLabelling(NewArguments).
bpLabelling(Arguments) :-
    member(A, Arguments),
    \+ isArgumentInBurdenOfProof(A),
    grounded::oneAttackIN(A), !,
    context_assert(out(A)),
    utils::subtract(Arguments, [A], NewArguments),
    bpLabelling(NewArguments).
bpLabelling(Arguments) :-
    context_active(Branch),
    mostGroundedBpUnd(Arguments, A),
    context_branch(Branch, _),
    context_assert(out(A)),
    utils::subtract(Arguments, [A], NewArguments),
    bpLabelling(NewArguments).
bpLabelling(Args) :-
    notLabelled(Args),
    findall(_, (member(A, Args), context_assert(und(A))), _).

notLabelled(Args) :-
    \+ (
        member(A, Args),
        (
            context_check(in(A));
            context_check(out(A));
            context_check(und(A))
        )
    ).


%==============================================================================
% BP LABELLING UTILITIES
%==============================================================================

isInBurdenOfProof(Conclusion) :-
    context_check(reifiedBp(Literals)),
    member(Conclusion, Literals), !.

isArgumentInBurdenOfProof([_, _, Conclusion, _, _]) :-
    isInBurdenOfProof(Conclusion).

filterBpDefeats :-
    findall(_, (
        context_check(attack(T, A, B, C)),
        filterBpDefeat(T, A, B, C)
    ), _).

filterBpDefeat(T, B, A, C) :-
    (T = rebut; T = undermine),
    isArgumentInBurdenOfProof(B),
    \+ superiority::superiorArgument(B, C),
    context_retract(attack(T, B, A, C)).

mostGroundedBpUnd(Arguments, Arg) :-
    member(Arg, Arguments),
    isArgumentInBurdenOfProof(Arg),
    \+ (
        member(A, Arguments), Arg \= A,
        isArgumentInBurdenOfProof(A),
        argumentChain(A, Arg)
    ).

argumentChain(A, A) :- !.
argumentChain(A, B) :-
    A \== B,
    context_check(attack(_, A, C, _)),
    argumentChain(C, B).

%==============================================================================
% BURDEN OF PROOF REIFICATION
%==============================================================================

reifyBurdenOfProofs :-
    findall(_, (
        context_check(abstractBp(AbstractBp)),
        fillTemplate(AbstractBp, R),
        \+ context_check(reifiedBp(R)),
        context_assert(reifiedBp(R))
    ), _).

/*
    Fill the template (first parameter) using arguments conclusions
*/

fillTemplate([], []).
fillTemplate([H|T], [[H]|R]) :-
    context_check(argument([_ , _, [P], _, _])),
    fillTemplate(T, R).
