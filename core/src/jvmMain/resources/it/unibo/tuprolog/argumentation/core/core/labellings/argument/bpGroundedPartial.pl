%==============================================================================
% BP PARTIAL LABELLING [ICLP]
%==============================================================================

% TODO: Check this implementation. It seems very confused

argumentLabelling(Rules, [Arguments, _, Supports], [SortedIn, SortedOut, SortedUnd]) :-
    bp_grounded::reifyBurdenOfProofs(Rules, Arguments, Bps),
    partialHBPLabelling(Bps, Supports, Arguments, [], [], [], In, Out, Und),
    utils::sort(In, SortedIn),
    utils::sort(Out, SortedOut),
    utils::sort(Und, SortedUnd).

partialHBPLabelling(_, _, [], IN_STAR, OUT_STAR, UND_STAR, IN_STAR, OUT_STAR, UND_STAR).
partialHBPLabelling(Bps, Supports, UND, IN_STAR, OUT_STAR, UND_STAR, ResultIN, ResultOUT, ResultUND) :-
    more_grounded_argument(UND, A),
    debug::writeDebug(['Evaluating ', A]),
    demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, [A], NewUnd, TempIN, TempOUT, TempUND),
    partialHBPLabelling(Bps, Supports, NewUnd, TempIN, TempOUT, TempUND, ResultIN, ResultOUT, ResultUND).

/*
    (a.i) BP(neg(φ)), and no argument B for neg(φ) such that A < B is IN*, and no A1,...An is OUT*
*/
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, OUT_STAR, UND_STAR) :-
	isComplementInBurdenOfProof(Bps, A),
	\+ findSupUndComplargument(A, UND, RESOLVING, _, _),
	noSuperiorComplementInSet(A, IN_STAR),
	\+ findUndSubargument(Supports, A, UND, RESOLVING, _, _),
	noSubArgumentInSet(Supports, A, OUT_STAR),
	debug::writeDebug(['Adding argument: ', A, ' to IN* (2.a.i)']),
    append(IN_STAR, [A], TempIN),
    utils::subtract(UND, [A], NewUnd).

/*
    (a.ii) not BP(neg(φ)) and every argument B for neg(φ) such that B(not <)A is OUT*, and every A1,...An is IN*
*/
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, OUT_STAR, UND_STAR) :-
	\+ isComplementInBurdenOfProof(Bps, A),
	\+ findSupOrEqualUndComplargument(A, UND, RESOLVING, _, _),
	allComplementInSet(A, OUT_STAR),
	\+ findUndSubargument(Supports, A, UND, RESOLVING, _, _),
	allSubArgumentInSet(Supports, A, IN_STAR),
	debug::writeDebug(['Adding argument: ', A, ' to IN* (2.a.ii)']),
    append(IN_STAR, [A], TempIN),
    utils::subtract(UND, [A], NewUnd).

/*
    (b.i.1) BP(φ) and exists an argument B for neg(φ) such that B(not <)A is not OUT*
*/
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	\+ findSupOrEqualUndComplargument(A, UND, RESOLVING, _, _),
	oneOutSuperiorOrEqualComplementFromSet(A, UND, OUT_STAR),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.i.1)']),
    append(OUT_STAR, [A], TempOUT),
    utils::subtract(UND, [A], NewUnd).

/*
    (b.i.2) BP(φ) and exist one of A1,...An is not IN*
*/
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	\+ findUndSubargument(Supports, A, UND, RESOLVING, _, _),
	oneOutSubArgumentFromSet(Supports, A, UND, IN_STAR),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.i.2)']),
    append(OUT_STAR, [A], TempOUT),
    utils::subtract(UND, [A], NewUnd).

/*
    (b.ii.1) not BP(φ) and an argument B for neg(φ) such A < B is IN*
*/
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	\+ findSupUndComplargument(A, UND, RESOLVING, _, _),
	oneInSuperiorOrEqualComplementFromSet(A, IN_STAR),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.ii.1)']),
    append(OUT_STAR, [A], TempOUT),
    utils::subtract(UND, [A], NewUnd).

/*
    (b.ii.2) not BP(φ) and one of A1,...An is OUT*
*/
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, IN_STAR, TempOUT, UND_STAR) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	\+ findUndSubargument(Supports, A, UND, RESOLVING, _, _),
	oneInSubArgumentFromSet(Supports, A, OUT_STAR),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.ii.2)']),
    append(OUT_STAR, [A], TempOUT),
    utils::subtract(UND, [A], NewUnd).


demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
    isComplementInBurdenOfProof(Bps, A),
	findSupUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Bps, Supports, Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	isComplementInBurdenOfProof(Bps, A),
	\+ findSupUndComplargument(A, UND, RESOLVING, _, _),
	noSuperiorComplementInSet(A, IN_STAR),
    findUndSubargument(Supports, A, UND, RESOLVING, NR, Sub),
    demonstration(Bps, Supports, Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
    \+ isComplementInBurdenOfProof(Bps, A),
	findAllUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Bps, Supports, Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	\+ isComplementInBurdenOfProof(Bps, A),
	\+ findAllUndComplargument(A, UND, RESOLVING, _, _),
	allComplementInSet(A, OUT_STAR),
    findUndSubargument(Supports, A, UND, RESOLVING, NR, Sub),
    demonstration(Bps, Supports, Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	findSupOrEqualUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Bps, Supports, Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	findUndSubargument(Supports, A, UND, RESOLVING, NR, Sub),
    demonstration(Bps, Supports, Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	findSupOrEqualUndComplargument(A, UND, RESOLVING, NR, Compl),
    demonstration(Bps, Supports, Compl, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, RESOLVING, NewUnd, TempIN, TempOUT, TempUND) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(Bps, A),
	findUndSubargument(Supports, A, UND, RESOLVING, NR, Sub),
    demonstration(Bps, Supports, Sub, UND, IN_STAR, OUT_STAR, UND_STAR, NR, NewUnd, TempIN, TempOUT, TempUND).

/*
    (c) A is labelled UND* otherwise.
*/
demonstration(Bps, Supports, A, UND, IN_STAR, OUT_STAR, UND_STAR, _, NewUnd, IN_STAR, OUT_STAR, TempUND) :-
	debug::writeDebug(['Adding argument: ', A, ' to UND* (2.c)']),
    append(UND_STAR, [A], TempUND),
    utils::subtract(UND, [A], NewUnd).


/*
    Load dependencies
*/
findUndSubargument(Supports, A, UND, RESOLVING, NEW_RESOLVING, Sub) :-
    member((Sub, A), Supports),
    member(Sub, UND),
    \+ member(Sub, RESOLVING),
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
    append(RESOLVING, [Compl], NEW_RESOLVING).

/*
    Conditions
*/

noSuperiorComplementInSet(Argument, Set) :-
    superiorComplArguments(Argument, LIST),
    noInWithEmptyCheck(LIST, Set).

noSubArgumentInSet(Supports, Argument, Set) :-
    allDirectsSubArguments(Supports, Argument, LIST),
    noInWithEmptyCheck(LIST, Set).

allComplementInSet(Argument, Set) :-
    allComplArguments(Argument, LIST),
    allInWithEmptyCheck(LIST, Set).

allSubArgumentInSet(Supports, Argument, Set) :-
    allDirectsSubArguments(Supports, Argument, LIST),
    allInWithEmptyCheck(LIST, Set).

oneOutSuperiorOrEqualComplementFromSet(Argument, UND, Set) :-
    superiorOrEqualComplArguments(Argument, LIST),
    oneOut(LIST, UND, Set).

oneOutSubArgumentFromSet(Supports, Argument, UND, Set) :-
    allDirectsSubArguments(Supports, Argument, LIST),
    oneOut(LIST, UND, Set).

oneInSuperiorOrEqualComplementFromSet(Argument, Set) :-
    superiorOrEqualComplArguments(Argument, LIST),
    oneIn(LIST, Set).

oneInSubArgumentFromSet(Supports, Argument, Set) :-
    allDirectsSubArguments(Supports, Argument, LIST),
    oneIn(LIST, Set).

/*
    Support
*/

allDirectsSubArguments(Supports, Argument, LIST) :-
    findall(Sub, member((Sub, Argument), Supports), LIST).

allComplArguments(Argument, LIST) :-
    findall(X, (
        complement(Argument, CA),
        findall([A, B, CA, I], argument([A, B, CA, I]), X)
    ), Y),
    utils::appendLists(Y, LIST).

superiorComplArguments(Argument, LIST) :-
    findall(X, (
        complement(Argument, CA),
        findall([A, B, CA, I], (argument([A, B, CA, I]), superiority::superiorArgument([A, B, CA, I], Argument)), X)
    ), Y),
    utils::appendLists(Y, LIST).

superiorOrEqualComplArguments(Argument, LIST) :-
    findall(X, (
        complement(Argument, CA),
        findall([A, B, CA, I], (argument([A, B, CA, I]), \+ superiority::superiorArgument(Argument, [A, B, CA, I])), X)
    ), Y),
    utils::appendLists(Y, LIST).

noInWithEmptyCheck([], _).
noInWithEmptyCheck(List, Target) :- noIn(List, Target).
noIn(List, Target) :-
    member(X, List),
    \+ member(X, Target).

allInWithEmptyCheck([], _).
allInWithEmptyCheck(List, Target) :- allIn(List, Target).
allIn(List, Target) :-
    member(_, List),
    \+ (member(Y, List), \+ member(Y, Target)).

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
% BP LABELLING UTILITIES
%==============================================================================

isComplementInBurdenOfProof(Bps, A) :-
    complement(A, Complement),
    bp_grounded::isInBurdenOfProof(Bps, Complement), !.

more_grounded_argument([], []).
more_grounded_argument([X], X).
more_grounded_argument([[L, _, _, _]|T], [L2, Q2, W2, I2]) :-
    more_grounded_argument(T, [L2, Q2, W2, I2]),
    length(L, LN1),
    length(L2, LN2),
    LN1 > LN2, !.
more_grounded_argument([A|_], A).

/*
    Get a conclusion complement
*/
complement([_, _, Conclusion, _], A) :-
    standard_af::conflict(Conclusion, A).
