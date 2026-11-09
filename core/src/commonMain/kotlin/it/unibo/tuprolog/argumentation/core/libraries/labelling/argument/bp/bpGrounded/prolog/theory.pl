%==============================================================================
% BP LABELLING [ICAIL]
%==============================================================================

%! locality(-Locality)
%  Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
%  label of an argument is taken to depend on the arguments connected to it, in either direction
locality(connected).

%! argumentLabelling
%  The labelling, in the active context, the burdens of persuasion (abstractBp/1) reified first. The labels are those
%  of the graph without the attacks failing for the burdens (see filterBpDefeats/0): computed in a branch of the active
%  one where they are dropped, which is released once its labels are read, and given to the graph as it is.
argumentLabelling :-
    reifyBurdenOfProofs,
    context_active(Graph),
    context_last(Mark),
    context_branch(Graph, _),
    filterBpDefeats,
    store::arguments(Arguments),
    bpLabelling(Arguments),
    findall(Label-Id, (member(Label, [in, out, und]), store::argumentLabelById(Label, Id)), Labels),
    context_checkout(Graph),
    context_drop_after(Mark, []),
    findall(_, (member(Label-Id, Labels), store::argumentById(Id, Argument), store::labelArgument(Label, Argument, Id)), _).

bpLabelling(Arguments) :-
    member(A, Arguments),
    allAttacksOUT(A), !,
    store::labelArgument(in, A),
    utils::subtract(Arguments, [A], NewArguments),
    bpLabelling(NewArguments).
bpLabelling(Arguments) :-
    member(A, Arguments),
    \+ isArgumentInBurdenOfProof(A),
    oneAttackIN(A), !,
    store::labelArgument(out, A),
    utils::subtract(Arguments, [A], NewArguments),
    bpLabelling(NewArguments).
bpLabelling(Arguments) :-
    context_active(Branch),
    mostGroundedBpUnd(Arguments, A), !,
    context_branch(Branch, _),
    store::labelArgument(out, A),
    utils::subtract(Arguments, [A], NewArguments),
    bpLabelling(NewArguments).
bpLabelling(Args) :-
    notLabelled(Args),
    findall(_, (member(A, Args), store::labelArgument(und, A)), _).

notLabelled(Args) :-
    \+ (
        member(A, Args),
        (
            store::argumentLabel(in, A);
            store::argumentLabel(out, A);
            store::argumentLabel(und, A)
        )
    ).


%==============================================================================
% BP LABELLING UTILITIES
%==============================================================================

%! groundedLabelling(+Arguments)
%  The grounded labelling of Arguments, the arguments not labelled yet, on the labels of the others (the
%  complete labelling with burdens of persuasion labels so the UND arguments of the partial one).
groundedLabelling(UND) :-
    member(A, UND),
    allAttacksOUT(A), !,
    utils::subtract(UND, [A], NewUND),
    store::labelArgument(in, A),
    groundedLabelling(NewUND).
groundedLabelling(UND) :-
    member(A, UND),
    oneAttackIN(A), !,
    utils::subtract(UND, [A], NewUND),
    store::labelArgument(out, A),
    groundedLabelling(NewUND).
groundedLabelling(Args) :- findall(_, (member(A, Args), store::labelArgument(und, A)), _).

%! allAttacksOUT(+Argument)
%  Every attacker of Argument is OUT.
allAttacksOUT(A) :-
    store::argumentId(A, Id),
    \+ (store::attacksById(B, Id), \+ store::argumentLabelById(out, B)).

%! oneAttackIN(+Argument)
%  An attacker of Argument is IN, or Argument attacks an IN argument.
oneAttackIN(A) :-
    store::argumentId(A, Id),
    (store::attacksById(B, Id) ; store::attacksById(Id, B)),
    store::argumentLabelById(in, B), !.

%! isInBurdenOfProof(+Conclusion)
%  A burden of persuasion is on Conclusion.
isInBurdenOfProof(Conclusion) :-
    context_check(reifiedBp(Literals)),
    member(Conclusion, Literals), !.

%! isArgumentInBurdenOfProof(+Argument)
%  A burden of persuasion is on the conclusion of Argument.
isArgumentInBurdenOfProof(Argument) :-
    store::conclusion(Argument, Conclusion),
    isInBurdenOfProof(Conclusion).

%! filterBpDefeats
%  The rebuts and undermines by an argument under a burden of persuasion dropped from the active branch, unless it is
%  superior to the argument attacked: a burden is met by winning, not by drawing.
filterBpDefeats :-
    findall(_, (
        store::directAttack(T, A, C),
        filterBpDefeat(T, A, C, C)
    ), _).

filterBpDefeat(T, B, A, C) :-
    (T = rebut; T = undermine),
    isArgumentInBurdenOfProof(B),
    \+ superiority::superiorArgument(B, C),
    store::dropAttack(T, B, A, C).

mostGroundedBpUnd(Arguments, Arg) :-
    member(Arg, Arguments),
    isArgumentInBurdenOfProof(Arg),
    \+ (
        member(A, Arguments), Arg \= A,
        isArgumentInBurdenOfProof(A),
        store::reaches(A, Arg)
    ).

%==============================================================================
% BURDEN OF PROOF REIFICATION
%==============================================================================

%! reifyBurdenOfProofs
%  The burdens of persuasion of the theory (see bp_grounded::burden/1) on the conclusions of the saved arguments, as
%  reifiedBp/1 in the context.
reifyBurdenOfProofs :-
    findall(_, (
        bp_grounded::burden(AbstractBp),
        fillTemplate(AbstractBp, R),
        context_absent(reifiedBp(R)),
        context_assert(reifiedBp(R))
    ), _).

% The literals of a burden, each the conclusion of a saved argument (instantiated jointly) unless it is ground
fillTemplate([], []).
fillTemplate([H|T], [[H]|R]) :-
    (ground(H) -> true ; store::argumentFor([H], _)),
    fillTemplate(T, R).
