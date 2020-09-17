% ----------------------------------------------------------------
% argumentLabelling.pl
% PIKA-lab
% Year: 2019
% ---------------------------------------------------------------

disableBPCompletion :-
    asserta(disableBPcompletion).

enableBPCompletion :-
    retractall(disableBPcompletion).

writeDemonstration([]) :-
    demonstration,
    write('\n').
writeDemonstration([X|T]) :-
    demonstration,
    write(X),
    writeDemonstration(T).
writeDemonstration(_).

argumentBPLabelling([IN, OUT, UND], [BPIN, BPOUT, BPUND]) :-
    reifyBurdenOfProofs(IN, OUT, UND),
    writeDemonstration(['=========================================>DEMONSTRATION']),
    ((disableBPcompletion, partialHBPLabelling(UND, IN, OUT, [], BPIN, BPOUT, BPUND));
    hbpComplete(go, IN, OUT, UND, BPIN, BPOUT, BPUND)),
    writeDemonstration(['=====================================>END DEMONSTRATION']).

%==============================================================================
% COMPLETE HBP LABELLING
%==============================================================================

hbpComplete(stop, IN, OUT, UND, IN, OUT, UND).
hbpComplete(_, IN, OUT, UND, BPIN, BPOUT, BPUND) :-
    writeDemonstration(['======================================================>']),
    partialHBPLabelling(UND, IN, OUT, [], BaseIN, BaseOUT, BaseUND),
    completeLabelling(BaseIN, BaseOUT, BaseUND, CompleteIN, CompleteOUT, CompleteUND),
    stopCondition(FLAG, IN, CompleteIN, OUT, CompleteOUT, UND, CompleteUND),
    hbpComplete(FLAG, CompleteIN, CompleteOUT, CompleteUND, BPIN, BPOUT, BPUND).

stopCondition(X, IN, CIN, OUT, COUT, UND, CUND) :-
    sort(IN, SIN),
    sort(CIN, SCIN),
    sort(OUT, SOUT),
    sort(COUT, SCOUT),
    sort(UND, SUND),
    sort(CUND, SCUND),
    stopCondition_sorted(X, SIN, SCIN, SOUT, SCOUT, SUND, SCUND).
stopCondition_sorted(stop, IN, IN, OUT, OUT, UND, UND).
stopCondition_sorted(go, _, _, _, _, _, _).

%==============================================================================
% PARTIAL HBP LABELLING
%==============================================================================

partialHBPLabelling([], IN_STAR, OUT_STAR, UND_STAR, IN_STAR, OUT_STAR, UND_STAR).
partialHBPLabelling(UND, IN_STAR, OUT_STAR, UND_STAR, ResultIN, ResultOUT, ResultUND) :-
    more_grounded_argument(UND, A),
    writeDemonstration(['Evaluating ', A]),
    demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, [A], NewUnd, TempIN, TempOUT, TempUND),
    partialHBPLabelling(NewUnd, TempIN, TempOUT, TempUND, ResultIN, ResultOUT, ResultUND).

/*
    (a.i) BP(neg(φ)), and no argument B for neg(φ) such that A < B is IN*, and no A1,...An is OUT*
*/

demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, OUT_STAR, UND_STAR) :-
	isComplementInBurdenOfProof(A),
	\+ findSupUndComplargument(A, UND, RESOLVING, _, _),
	noSuperiorComplementInSet(A, IN_STAR),
	\+ findUndSubargument(A, UND, RESOLVING, _, _),
	noSubArgumentInSet(A, OUT_STAR),
	writeDemonstration(['Adding argument: ', A, ' to IN* (2.a.i)']),
    append(IN_STAR, [A], TempIN),
    subtract(UND, [A], NewUnd).

/*
    (a.ii) not BP(neg(φ)) and every argument B for neg(φ) such that B(not <)A is OUT*, and every A1,...An is IN*
*/
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, OUT_STAR, UND_STAR) :-
	\+ isComplementInBurdenOfProof(A),
	\+ findSupOrEqualUndComplargument(A, UND, RESOLVING, _, _),
	allComplementInSet(A, OUT_STAR),
	\+ findUndSubargument(A, UND, RESOLVING, _, _),
	allSubArgumentInSet(A, IN_STAR),
	writeDemonstration(['Adding argument: ', A, ' to IN* (2.a.ii)']),
    append(IN_STAR, [A], TempIN),
    subtract(UND, [A], NewUnd).

/*
    (b.i.1) BP(φ) and exists an argument B for neg(φ) such that B(not <)A is not OUT*
*/
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	isArgumentInBurdenOfProof(A),
	\+ findSupOrEqualUndComplargument(A, UND, RESOLVING, _, _),
	oneOutSuperiorOrEqualComplementFromSet(A, UND, OUT_STAR),
    writeDemonstration(['Adding argument: ', A, ' to OUT* (2.b.i.1)']),
    append(OUT_STAR, [A], TempOUT),
    subtract(UND, [A], NewUnd).

/*
    (b.i.2) BP(φ) and exist one of A1,...An is not IN*
*/
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	isArgumentInBurdenOfProof(A),
	\+ findUndSubargument(A, UND, RESOLVING, _, _),
	oneOutSubArgumentFromSet(A, UND, IN_STAR),
    writeDemonstration(['Adding argument: ', A, ' to OUT* (2.b.i.2)']),
    append(OUT_STAR, [A], TempOUT),
    subtract(UND, [A], NewUnd).

/*
    (b.ii.1) not BP(φ) and an argument B for neg(φ) such A < B is IN*
*/
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	\+ isArgumentInBurdenOfProof(A),
	\+ findSupUndComplargument(A, UND, RESOLVING, _, _),
	oneInSuperiorOrEqualComplementFromSet(A, IN_STAR),
    writeDemonstration(['Adding argument: ', A, ' to OUT* (2.b.ii.1)']),
    append(OUT_STAR, [A], TempOUT),
    subtract(UND, [A], NewUnd).

/*
    (b.ii.2) not BP(φ) and one of A1,...An is OUT*
*/
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	\+ isArgumentInBurdenOfProof(A),
	\+ findUndSubargument(A, UND, RESOLVING, _, _),
	oneInSubArgumentFromSet(A, OUT_STAR),
    writeDemonstration(['Adding argument: ', A, ' to OUT* (2.b.ii.2)']),
    append(OUT_STAR, [A], TempOUT),
    subtract(UND, [A], NewUnd).


demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
    isComplementInBurdenOfProof(A),
	findSupUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	isComplementInBurdenOfProof(A),
	\+ findSupUndComplargument(A, UND, RESOLVING, _, _),
	noSuperiorComplementInSet(A, IN_STAR),
    findUndSubargument(A, UND, RESOLVING, NR, Sub),
    demonstration(Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
    \+ isComplementInBurdenOfProof(A),
	findAllUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	\+ isComplementInBurdenOfProof(A),
	\+ findAllUndComplargument(A, UND, RESOLVING, _, _),
	allComplementInSet(A, OUT_STAR),
    findUndSubargument(A, UND, RESOLVING, NR, Sub),
    demonstration(Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	isArgumentInBurdenOfProof(A),
	findSupOrEqualUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	isArgumentInBurdenOfProof(A),
	findUndSubargument(A, UND, RESOLVING, NR, Sub),
    demonstration(Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	\+ isArgumentInBurdenOfProof(A),
	findSupOrEqualUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	\+ isArgumentInBurdenOfProof(A),
	findUndSubargument(A, UND, RESOLVING, NR, Sub),
    demonstration(Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

/*
    (c) A is labelled UND* otherwise.
*/
demonstration(A, UND, IN_STAR, OUT_STAR, UND_STAR, _, NewUnd, IN_STAR, OUT_STAR, TempUND) :-
	writeDemonstration(['Adding argument: ', A, ' to UND* (2.c)']),
    append(UND_STAR, [A], TempUND),
    subtract(UND, [A], NewUnd).

/*
    Load dependencies
*/
findUndSubargument(A, UND, RESOLVING, NEW_RESOLVING, Sub) :-
    support(Sub, A),
    member(Sub, UND),
    \+ member(Sub, RESOLVING),
    % writeDemonstration(['Sub -> ', Sub, ' of ', A]),
    append(RESOLVING, [Sub], NEW_RESOLVING).

findAllUndComplargument(A, UND, RESOLVING, NEW_RESOLVING, Compl) :-
    allComplArguments(A, List),
    findAcceptable(A, List, UND, RESOLVING, NEW_RESOLVING, Compl).

findSupUndComplargument(A, UND, RESOLVING, NEW_RESOLVING, Compl) :-
    superiorComplArguments(A, List),
    findAcceptable(A, List, UND, RESOLVING, NEW_RESOLVING, Compl).

findSupOrEqualUndComplargument(A, UND, RESOLVING, NEW_RESOLVING, Compl) :-
    superiorOrEqualComplArguments(A, List),
    findAcceptable(A, List, UND, RESOLVING, NEW_RESOLVING, Compl).

findAcceptable(A, List, UND, RESOLVING, NEW_RESOLVING, Compl) :-
    member(Compl, List),
    member(Compl, UND),
    \+ member(Compl, RESOLVING),
    % writeDemonstration(['Compl -> ', Compl, ' of ', A]),
    append(RESOLVING, [Compl], NEW_RESOLVING).

/*
    Conditions
*/

noSuperiorComplementInSet(Argument, Set) :-
    superiorComplArguments(Argument, LIST),
    noInWithEmptyCheck(LIST, Set).

noSubArgumentInSet(Argument, Set) :-
    allDirectsSubArguments(Argument, LIST),
    noInWithEmptyCheck(LIST, Set).

allComplementInSet(Argument, Set) :-
    allComplArguments(Argument, LIST),
    allInWithEmptyCheck(LIST, Set).

allSubArgumentInSet(Argument, Set) :-
    allDirectsSubArguments(Argument, LIST),
    allInWithEmptyCheck(LIST, Set).

oneOutSuperiorOrEqualComplementFromSet(Argument, UND, Set) :-
    superiorOrEqualComplArguments(Argument, LIST),
    oneOut(LIST, UND, Set).

oneOutSubArgumentFromSet(Argument, UND, Set) :-
    allDirectsSubArguments(Argument, LIST),
    oneOut(LIST, UND, Set).

oneInSuperiorOrEqualComplementFromSet(Argument, Set) :-
    superiorOrEqualComplArguments(Argument, LIST),
    oneIn(LIST, Set).

oneInSubArgumentFromSet(Argument, Set) :-
    allDirectsSubArguments(Argument, LIST),
    oneIn(LIST, Set).

/*
    Support
*/

allDirectsSubArguments(Argument, LIST) :-
    findall(Sub, support(Sub, Argument), LIST).

allComplArguments(Argument, LIST) :-
    complement(Argument, CA),
    findall([A, B, CA], argument([A, B, CA]), LIST).

superiorComplArguments(Argument, LIST) :-
    complement(Argument, CA),
    findall([A, B, CA], (argument([A, B, CA]), superiorArgument([A, B, CA], Argument)), LIST).

superiorOrEqualComplArguments(Argument, LIST) :-
    complement(Argument, CA),
    findall([A, B, CA], (argument([A, B, CA]), \+ superiorArgument(Argument, [A, B, CA])), LIST).


noInWithEmptyCheck([], _).
noInWithEmptyCheck(List, Target) :- noIn(List, Target).
noIn(List, Target) :-
    member(X, List),
    \+ member(X, Target).

allInWithEmptyCheck([], _).
allInWithEmptyCheck(List, Target) :- allIn(List, Target).
allIn(List, Target) :-
    member(X, List),
    \+ (member(X, List), \+ member(X, Target)).

oneInWithEmptyCheck([], _).
oneInWithEmptyCheck(List, Target) :- oneIn(List, Target).
oneIn(List, Target) :-
    member(X, List),
    member(X, Target).

oneOutWithEmptyCheck([], _, _).
oneOutWithEmptyCheck(List, UND, Target) :- oneOut(List, UND, Target).
oneOut(List, UND, Target) :-
    member(X, List),
    \+ member(X, UND),
    \+ member(X, Target).

%==============================================================================
% COMPLETE LABELLING
%==============================================================================

completeLabelling(IN, OUT, UND, ResultIN, ResultOUT, ResultUND) :-
    findoneIn(IN, OUT, UND, A),
    writeDemonstration(['Adding argument: ', A, ' to IN* (4.4)']),
    append(IN, [A], NewIN),
    subtract(UND, [A], NewUnd),
    completeLabelling(NewIN, OUT, NewUnd, ResultIN, ResultOUT, ResultUND).
completeLabelling(IN, OUT, UND, ResultIN, ResultOUT, ResultUND) :-
    findoneOut(IN, OUT, UND, A),
    writeDemonstration(['Adding argument: ', A, ' to OUT* (4.4)']),
    append(OUT, [A], NewOUT),
    subtract(UND, [A], NewUnd),
    completeLabelling(IN, NewOUT, NewUnd, ResultIN, ResultOUT, ResultUND).
completeLabelling(IN, OUT, UND, IN, OUT, UND).

findoneIn(IN, OUT, UND, A):-
    member(A, UND),
    completeIn(A, IN, OUT).

findoneOut(IN, OUT, UND, A):-
    member(A, UND),
    completeOut(A, IN, OUT).

completeIn(A, _, OUT) :- checkOutAttackers(A, OUT).
/*
    If an attack exists, it should come from an OUT argument
*/
checkOutAttackers(A, OUT) :-
    \+ ( attack(B, A), \+ ( member(B, OUT)) ).


completeOut(A, IN, _) :- checkInAttacker(A, IN).
completeOut(A, IN, _) :- checkInAttecked(A, IN).
/*
    Find an attack, if exists, from an IN argument, then ends
*/
checkInAttacker(A, IN) :-
    attack(B, A),
    member(B, IN), !.

/*
    If A attacks an IN argument, then A is OUT
*/
checkInAttecked(A, IN) :-
    attack(A, B),
    member(B, IN), !.

%==============================================================================
% HBP LABELLING UTILITIES
%==============================================================================

/*
    Checks Burden of proof membership
*/
isInBurdenOfProof(Concl) :-
    reifiedBp(Literals),
    member(Concl, Literals), !.

isComplementInBurdenOfProof(A) :-
    complement(A, Compl),
    isInBurdenOfProof(Compl).

isArgumentInBurdenOfProof([_, _, Concl]) :-
    isInBurdenOfProof(Concl).

more_grounded_argument([], []).
more_grounded_argument([X], X).
more_grounded_argument([[L,_,_]|T], [L2,Q2,W2]) :-
    more_grounded_argument(T, [L2,Q2,W2]),
    length(L, LN1),
    length(L2, LN2),
    LN1 > LN2.
more_grounded_argument([[L,Q,W]|_], [L,Q,W]).

/*
    Get a conclusion complement ([P] -> [neg, P])
*/
complement([_, _, [neg|A]], A).
complement([_, _, [A]], ['neg',A]).

%==============================================================================
% BURDEN OF PROOF REIFICATION
%==============================================================================

reifyBurdenOfProofs(IN, OUT, UND) :-
    extractConclusions(IN, OUT, UND, Conclusions),
    computeBp(Conclusions).

extractConclusions(IN, OUT, UND, SL) :-
    findall(Conc, member([_, _, Conc], IN), In),
    findall(Conc, member([_, _, Conc], OUT), Out),
    findall(Conc, member([_, _, Conc], UND), Und),
    appendLists([In, Out, Und], L),
    sort(L, SL).

computeBp(Conclusions) :-
    abstractBp(AbstractBp),
    fillTemplate(AbstractBp, Conclusions, R),
    \+ reifiedBp(R),
    asserta(reifiedBp(R)),
    computeBp(Conclusions).

computeBp(_).

/*
    Fill the template (first parameter) using predicates belonging
    to the second list (second parameter)
*/
fillTemplate([], _, []).
fillTemplate([H|T], C, [H|R]) :- member(H, C), fillTemplate(T, C, R).