%==============================================================================
% BP PARTIAL LABELLING [ICLP]
%==============================================================================

% One labelling: the demonstrations below leave choice points, which the cut of argumentLabelling/0 drops once every
% argument is labelled, each once; an argument is weighed against every argument for a complement of its conclusion
% (complementArgument/2), whatever the conflicts of the theory (see BpGroundedPartialTest)

%! locality(-Locality)
%  Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
%  label of an argument is taken to depend on the arguments connected to it, in either direction
locality(connected).

%! argumentLabelling
%  The labelling, in the active context.
argumentLabelling :-
    bp_grounded::reifyBurdenOfProofs,
    store::arguments(Arguments),
    partialBpLabelling(Arguments), !.

%! partialBpLabelling(+Arguments)
%  The labels of Arguments, the most grounded first, deciding first the arguments the label of each depends on.
partialBpLabelling([]).
partialBpLabelling(Arguments) :-
    mostGroundedArgument(Arguments, A),
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
        store::argumentLabel(in, B)
    ),
    \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
    \+ (
        store::support(X, A),
        store::argumentLabel(out, X)
    ),
    store::labelArgument(in, A).

/*
    (a.ii) not BP(neg(φ)) and every argument B for neg(φ) such that B(not <)A is OUT*, and every A1,...An is IN*
*/
demonstration(Arguments, A, Resolving, A) :-
    \+ isComplementInBurdenOfProof(A),
    \+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
    \+ (
        superiorOrEqualComplementArgument(A, B),
        \+ store::argumentLabel(out, B)
    ),
    \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
    \+ (
        store::support(X, A),
        \+ store::argumentLabel(in, X)
    ),
    store::labelArgument(in, A).

/*
    (b.i.1) BP(φ) and exists an argument B for neg(φ) such that B(not <)A is not OUT*
*/
demonstration(Arguments, A, Resolving, A) :-
    bp_grounded::isArgumentInBurdenOfProof(A),
    \+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
    superiorOrEqualComplementArgument(A, B),
    \+ store::argumentLabel(out, B),
    store::labelArgument(out, A).

/*
    (b.i.2) BP(φ) and exist one of A1,...An is not IN*
*/
demonstration(Arguments, A, Resolving, A) :-
    bp_grounded::isArgumentInBurdenOfProof(A),
    \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
    store::support(X, A),
    \+ store::argumentLabel(in, X),
    store::labelArgument(out, A).

/*
    (b.ii.1) not BP(φ) and an argument B for neg(φ) such A < B is IN*
*/
demonstration(Arguments, A, Resolving, A) :-
    \+ bp_grounded::isArgumentInBurdenOfProof(A),
    \+ notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, _),
    superiorComplementArgument(A, B),
    store::argumentLabel(in, B),
    store::labelArgument(out, A).

/*
    (b.ii.2) not BP(φ) and one of A1,...An is OUT*
*/
demonstration(Arguments, A, Resolving, A) :-
    \+ bp_grounded::isArgumentInBurdenOfProof(A),
    \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
    store::support(X, A),
    store::argumentLabel(out, X),
    store::labelArgument(out, A).

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
        store::argumentLabel(in, X)
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
        \+ store::argumentLabel(out, X)
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
    store::labelArgument(und, A).

%==============================================================================
% BP LABELLING UTILITIES
%==============================================================================

isComplementInBurdenOfProof(A) :-
    complement(A, Complement),
    bp_grounded::isInBurdenOfProof(Complement), !.

mostGroundedArgument([], []).
mostGroundedArgument([X], X).
mostGroundedArgument([A|T], Best) :-
    mostGroundedArgument(T, Best),
    store::argumentRules(A, RulesA),
    store::argumentRules(Best, RulesBest),
    length(RulesA, LN1),
    length(RulesBest, LN2),
    LN1 > LN2, !.
mostGroundedArgument([A|_], A).

/*
    Get a conclusion complement
*/
complement(Argument, Complement) :-
    store::conclusion(Argument, Conclusion),
    conflict::between(Conclusion, Complement).

notEvaluatedSubArgument(Arguments, A, Resolving, B) :-
    store::support(B, A),
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

superiorComplementArgument(A, B) :-
    complementArgument(A, B),
    superiority::superiorArgument(B, A).

superiorOrEqualComplementArgument(A, B) :-
    complementArgument(A, B),
    \+ superiority::superiorArgument(A, B).

% B is a saved argument for the complement of the conclusion of A
complementArgument(A, B) :-
    complement(A, Complement),
    store::argumentFor(Complement, B).
