%==============================================================================
% BP LABELLING [ICAIL]
%==============================================================================

argumentBPLabelling([Arguments, Attacks, _], [BPIN, BPOUT, BPUND]) :-
    reifyBurdenOfProofs(Arguments, [], []),
    once(filterBpDefeat(Attacks, FilteredAttacks)),
    smartBpLabelling(Arguments, FilteredAttacks, [], [], [], BPIN, BPOUT, BPUND), !.

filterBpDefeat([], []).
filterBpDefeat([(T, B, A)|Attacks], FilteredAttacks) :-
    (T = rebut; T = undermine),
    isArgumentInBurdenOfProof(B),
    attack(T, B, A, C),
    \+ superiorArgument(B, A, C),
    filterBpDefeat(Attacks, FilteredAttacks).
filterBpDefeat([A|Attacks], [A|FilteredAttacks]) :-
    filterBpDefeat(Attacks, FilteredAttacks).

smartBpLabelling(Arguments, Attacks, IN, OUT, UND, ResultIN, ResultOUT, ResultUND) :-
    member(A, Arguments),
    allAttacksOUT(Attacks, A, OUT),
    subtract(Arguments, [A], NewArguments),
    smartBpLabelling(NewArguments, Attacks, [A|IN], OUT, UND, ResultIN, ResultOUT, ResultUND).
smartBpLabelling(Arguments, Attacks, IN, OUT, UND, ResultIN, ResultOUT, ResultUND) :-
    member(A, Arguments),
    \+ isArgumentInBurdenOfProof(A),
    oneAttackIN(Attacks, A, IN),
    subtract(Arguments, [A], NewArguments),
    smartBpLabelling(NewArguments, Attacks, IN, [A|OUT], UND, ResultIN, ResultOUT, ResultUND).
smartBpLabelling(Arguments, Attacks, IN, OUT, UND, ResultIN, ResultOUT, ResultUND) :-
    mostGroundedBpUnd(Arguments, Attacks, A),
    subtract(Arguments, [A], NewArguments),
    smartBpLabelling(NewArguments, Attacks, IN, [A|OUT], UND, ResultIN, ResultOUT, ResultUND).
smartBpLabelling(Arguments, _, IN, OUT, _, IN, OUT, Arguments).

mostGroundedBpUnd(Arguments, Attacks, Arg) :-
    member(Arg, Arguments),
    isArgumentInBurdenOfProof(Arg),
    \+ (
        (member(A, Arguments), Arg \= A, isArgumentInBurdenOfProof(A)),
        argumentChain(A, Arg, Attacks)
    ).

%==============================================================================
% BP LABELLING UTILITIES
%==============================================================================

/*
    Checks Burden of proof membership
*/
isInBurdenOfProof(Concl) :-
    reifiedBp(Literals),
    member(Concl, Literals), !.

isArgumentInBurdenOfProof([_, _, Concl]) :-
    isInBurdenOfProof(Concl).

%==============================================================================
% BURDEN OF PROOF REIFICATION
%==============================================================================

reifyBurdenOfProofs(Rules, Arguments, Bps) :-
    extractConclusions(Arguments, Conclusions),
    computeBp(Rules, Conclusions, [], Bps).

extractConclusions(Arguments, Conclusions) :-
    findall(Conc, member([_, _, Conc, _], Arguments), Conc),
    utils::sort(Conc, Conclusions).

computeBp(Rules, Conclusions, TempBps, Bps) :-
    member(abstractBp(AbstractBp), Rules),
    fillTemplate(AbstractBp, Conclusions, R),
    \+ member(reifiedBp(R), TempBps),
    computeBp(Rules, Conclusions, [reifiedBp(R)|TempBps], Bps).
computeBp(_, _, Bps, Bps).

/*
    Fill the template (first parameter) using predicates belonging
    to the second list (second parameter)
*/
fillTemplate([], _, []).
fillTemplate([H|T], C, [H|R]) :- member(H, C), fillTemplate(T, C, R).
