%==============================================================================
% BP PARTIAL LABELLING [ICLP]
%==============================================================================

% TODO: handle branching and cuts

argumentLabelling :-
    bp_grounded::reifyBurdenOfProofs,
    findall(X, context_check(argument(X)), Arguments),
    partialBpLabelling(Arguments), !.

partialBpLabelling([]).
partialBpLabelling(Arguments) :-
    more_grounded_argument(Arguments, A),
    debug::writeDebug(['Evaluating ', A]),
    demonstration(Arguments, A, [A], Evaluated),
    utils::subtract(Arguments, [Evaluated], NewArgs),
    partialBpLabelling(NewArgs).

/*
    (a.i) BP(neg(φ)), and no argument B for neg(φ) such that A < B is IN*, and no A1,...An is OUT*
*/
demonstration(Arguments, A, Resolving, A) :-
	isComplementInBurdenOfProof(A),
	\+ notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, _),
	\+ (
	    superiorComplementArgument(A, B),
	    context_check(in(B))
	),
	\+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
	\+ (
	    context_check(support(X, A)),
	    context_check(out(X))
	),
	debug::writeDebug(['Adding argument: ', A, ' to IN* (2.a.i)']),
    context_assert(in(A)).

/*
    (a.ii) not BP(neg(φ)) and every argument B for neg(φ) such that B(not <)A is OUT*, and every A1,...An is IN*
*/
demonstration(Arguments, A, Resolving, A) :-
	\+ isComplementInBurdenOfProof(A),
	\+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
	\+ (
        superiorOrEqualComplementArgument(A, B),
        \+ context_check(out(B))
    ),
	\+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
	\+ (
        context_check(support(X, A)),
        \+ context_check(in(X))
    ),
	debug::writeDebug(['Adding argument: ', A, ' to IN* (2.a.ii)']),
    context_assert(in(A)).

/*
    (b.i.1) BP(φ) and exists an argument B for neg(φ) such that B(not <)A is not OUT*
*/
demonstration(Arguments, A, Resolving, A) :-
	bp_grounded::isArgumentInBurdenOfProof(A),
	\+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
	superiorOrEqualComplementArgument(A, B),
	\+ context_check(out(B)),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.i.1)']),
    context_assert(out(A)).

/*
    (b.i.2) BP(φ) and exist one of A1,...An is not IN*
*/
demonstration(Arguments, A, Resolving, A) :-
	bp_grounded::isArgumentInBurdenOfProof(A),
	\+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
	context_check(support(X, A)),
    \+ context_check(in(X)),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.i.2)']),
    context_assert(out(A)).

/*
    (b.ii.1) not BP(φ) and an argument B for neg(φ) such A < B is IN*
*/
demonstration(Arguments, A, Resolving, A) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(A),
	\+ notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, _),
	superiorComplementArgument(A, B),
	context_check(in(B)),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.ii.1)']),
    context_assert(out(A)).

/*
    (b.ii.2) not BP(φ) and one of A1,...An is OUT*
*/
demonstration(Arguments, A, Resolving, A) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(A),
	\+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
	context_check(support(X, A)),
    context_check(out(X)),
    debug::writeDebug(['Adding argument: ', A, ' to OUT* (2.b.ii.2)']),
    context_assert(out(A)).

/*
    If above definitions do not apply try to decide on the arguments on which they depend
*/
demonstration(Arguments, A, Resolving, Evaluated) :-
    isComplementInBurdenOfProof(A),
	notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

demonstration(Arguments, A, Resolving, Evaluated) :-
	isComplementInBurdenOfProof(A),
	\+ notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, _),
	\+ (
        superiorComplementArgument(A, X),
        context_check(in(X))
    ),
    notEvaluatedSubArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

demonstration(Arguments, A, Resolving, Evaluated) :-
    \+ isComplementInBurdenOfProof(A),
	notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

demonstration(Arguments, A, Resolving, Evaluated) :-
	\+ isComplementInBurdenOfProof(A),
	\+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
	\+ (
        superiorOrEqualComplementArgument(A, X),
        \+ context_check(out(X))
    ),
    notEvaluatedSubArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

demonstration(Arguments, A, Resolving, Evaluated) :-
	bp_grounded::isArgumentInBurdenOfProof(A),
	notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

demonstration(Arguments, A, Resolving, Evaluated) :-
	bp_grounded::isArgumentInBurdenOfProof(A),
	notEvaluatedSubArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

demonstration(Arguments, A, Resolving, Evaluated) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(A),
	notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

demonstration(Arguments, A, Resolving, Evaluated) :-
	\+ bp_grounded::isArgumentInBurdenOfProof(A),
	notEvaluatedSubArgument(Arguments, A, Resolving, B),
    demonstration(Arguments, B, [B|Resolving], Evaluated).

/*
    (c) A is labelled UND* otherwise.
*/
demonstration(Arguments, A, Resolving, A) :-
	debug::writeDebug(['Adding argument: ', A, ' to UND* (2.c)']),
    context_assert(und(A)).

%==============================================================================
% BP LABELLING UTILITIES
%==============================================================================

isComplementInBurdenOfProof(A) :-
    complement(A, Complement),
    bp_grounded::isInBurdenOfProof(Complement), !.

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
complement([_, _, Conclusion, _, _], A) :-
    standard_af::conflict(Conclusion, A).

notEvaluatedSubArgument(Arguments, A, Resolving, B) :-
    context_check(support(B, A)),
    member(B, Arguments),
    \+ member(B, Resolving).

notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, B) :-
    superiorComplementArgument(A, B),
    member(B, Arguments),
    \+ member(B, Resolving).

notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B) :-
    superiorOrEqualComplementArgument(A, B),
    member(B, Arguments),
    \+ member(B, Resolving).

superiorComplementArgument(A, [A, B, CA, BB, I]) :-
    complement(A, CA),
    context_check(argument([R, B, CA, BB, I])),
    superiority::superiorArgument([R, B, CA, BB, I], A).

superiorOrEqualComplementArgument(A, [R, B, CA, BB, I]) :-
    complement(A, CA),
    context_check(argument([R, B, CA, BB, I])),
    \+ superiority::superiorArgument(A, [R, B, CA, BB, I]).
