%==============================================================================
% BP LABELLING [ICAIL]
%==============================================================================

argumentLabelling(Rules, [Arguments, Attacks, _], [SortedIn, SortedOut, SortedUnd]) :-
    reifyBurdenOfProofs(Rules, Arguments, Bps),
    filterBpDefeat(Bps, Attacks, FilteredAttacks),
    smartBpLabelling(Bps, Arguments, FilteredAttacks, [], [], [], In, Out, Und),
    utils::sort(In, SortedIn),
    utils::sort(Out, SortedOut),
    utils::sort(Und, SortedUnd).

smartBpLabelling(Bps, Arguments, Attacks, In, Out, Und, ResultIn, ResultOut, ResultUnd) :-
    member(A, Arguments),
    grounded::allAttacksOUT(Attacks, A, Out),
    utils::subtract(Arguments, [A], NewArguments),
    smartBpLabelling(Bps, NewArguments, Attacks, [A|In], Out, Und, ResultIn, ResultOut, ResultUnd).
smartBpLabelling(Bps, Arguments, Attacks, In, Out, Und, ResultIn, ResultOut, ResultUnd) :-
    member(A, Arguments),
    \+ isArgumentInBurdenOfProof(Bps, A),
    grounded::oneAttackIN(Attacks, A, In),
    utils::subtract(Arguments, [A], NewArguments),
    smartBpLabelling(Bps, NewArguments, Attacks, In, [A|Out], Und, ResultIn, ResultOut, ResultUnd).
smartBpLabelling(Bps, Arguments, Attacks, In, Out, Und, ResultIn, ResultOut, ResultUnd) :-
    mostGroundedBpUnd(Bps, Arguments, Attacks, A),
    utils::subtract(Arguments, [A], NewArguments),
    smartBpLabelling(Bps, NewArguments, Attacks, In, [A|Out], Und, ResultIn, ResultOut, ResultUnd).
smartBpLabelling(_, Arguments, _, In, Out, _, In, Out, Arguments).

%==============================================================================
% BP LABELLING UTILITIES
%==============================================================================

isInBurdenOfProof(Bps, Conclusion) :-
    member(reifiedBp(Literals), Bps),
    member(Conclusion, Literals), !.


isArgumentInBurdenOfProof(Bps, [_, _, Conclusion, _]) :-
    isInBurdenOfProof(Bps, Conclusion).


filterBpDefeat(_, [], []).
filterBpDefeat(Bps, [(T, B, A, C)|Attacks], FilteredAttacks) :-
    (T = rebut; T = undermine),
    isArgumentInBurdenOfProof(Bps, B),
    \+ superiority::superiorArgument(B, C),
    filterBpDefeat(Bps, Attacks, FilteredAttacks), !.
filterBpDefeat(Bps, [A|Attacks], [A|FilteredAttacks]) :-
    filterBpDefeat(Bps, Attacks, FilteredAttacks).


mostGroundedBpUnd(Bps, Arguments, Attacks, Arg) :-
    member(Arg, Arguments),
    isArgumentInBurdenOfProof(Bps, Arg),
    \+ (
        (member(A, Arguments), Arg \= A,
        isArgumentInBurdenOfProof(Bps, A)),
        argumentChain(A, Arg, Attacks)
    ).


argumentChain(A, A, _) :- !.
argumentChain(A, B, Attacks) :-
    A \== B,
    member((_, A, C, _), Attacks),
    argumentChain(C, B, Attacks).

%==============================================================================
% BURDEN OF PROOF REIFICATION
%==============================================================================

reifyBurdenOfProofs(Rules, Arguments, Bps) :-
    extractConclusions(Arguments, Conclusions),
    computeBp(Rules, Conclusions, [], Bps).

extractConclusions(Arguments, Conclusions) :-
    findall(Conclusion, member([_, _, Conclusion, _], Arguments), AllConclusions),
    utils::sort(AllConclusions, Conclusions).

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
