%! modifyArgumentationGraph
%  The arguments for the burdens met, and their attacks with the burdened arguments.
modifyArgumentationGraph :-
    findall((Bp, Burdened), (
        store::claim(Bp, bp(Statement)),
        store::argument(Bp),
        store::claim(Burdened, Statement),
        store::argument(Burdened)
    ), BurdenedPairs),
    checkBpArguments(BurdenedPairs).

checkBpArguments([]) :- !.
checkBpArguments(BurdenedPairs) :-
    createBpArguments(BurdenedPairs, BpPairs),
    createBpAttacks(BpPairs).

% The conflict argument of a burdened argument for each argument for a burden of proof on it (its rules those
% of both, after artificial): supported by that burden argument, so attacked as it is (see the module
% store), the other burden arguments standing on their own
createBpArguments([], []).
createBpArguments([(Bp, Burdened)|Others], [(Conflict, Burdened)|BpPairs]) :-
    store::argumentRules(Bp, BpRules),
    store::argumentRules(Burdened, Rules),
    append(BpRules, Rules, AllRules),
    store::conclusion(Burdened, Conclusion),
    store::infoParts(Info, [artificial], [artificial], []),
    store::newArgument([artificial|AllRules], artificial, [-burdmet(Conclusion)], [], Info, Conflict),
    store::saveArgument([-burdmet(Conclusion)], Conflict),
    store::saveSupport(Bp, Conflict),
    createBpArguments(Others, BpPairs).

createBpAttacks(BpPairs) :-
    generateBpsEvaluationChain(BpPairs, OrderedBpPairs),
    evaluateBurdenedArgs(OrderedBpPairs).

% An evaluation order for the pairs, based on the attacks between them: if a node A is reached from a node B
% through attacks, B is evaluated before A. Cycles are not admitted.
generateBpsEvaluationChain([], []).
generateBpsEvaluationChain([BpPair|BpPairs], OrderedBpPairs) :-
    generateBpsEvaluationChain(BpPairs, TempOrderedBpPairs),
    insertBpPair(BpPair, TempOrderedBpPairs, OrderedBpPairs).

insertBpPair(BpPair, [], [BpPair]).
insertBpPair((Bp, Burdened), [(BpL, BurdenedL)|Others], [(BpL, BurdenedL)|Return]) :-
    store::reaches(BurdenedL, Burdened),
    insertBpPair((Bp, Burdened), Others, Return), !.
insertBpPair((Bp, Burdened), [(BpL, BurdenedL)|Others], [(Bp, Burdened)|[(BpL, BurdenedL)|Others]]).

% Each pair in turn: the burdened argument rebuts the conflict argument if the grounded labelling labels it IN,
% otherwise it is rebutted
evaluateBurdenedArgs([]).
evaluateBurdenedArgs([(Bp, Burdened)|Others]) :-
    contextActive(X),
    grounded:::argumentLabelling,
    statusToAttack((Bp, Burdened), attack(T, A, B, C)),
    contextCheckout(X),
    store::saveAttack(T, A, B, C),
    evaluateBurdenedArgs(Others).

statusToAttack((Bp, Burdened), attack(bprebut, Burdened, Bp, Bp)) :-
    store::argumentLabel(in, Burdened).
statusToAttack((Bp, Burdened), attack(bprebut, Bp, Burdened, Burdened)) :-
    \+ store::argumentLabel(in, Burdened).

%==============================================================================
% QUERY MODE
%==============================================================================

%! queryDependencies(+Argument, -Statement)
%  A burden of proof on a statement is itself a conclusion (bp/1), related to the arguments for the statement.
queryDependencies(Argument, bp(Claim)) :-
    store::claim(Argument, Claim).

%! queryDependants(+Argument, -Statement)
%  The arguments for a statement depend on the burden of proof on it.
queryDependants(Argument, Claim) :-
    store::claim(Argument, bp(Claim)).
