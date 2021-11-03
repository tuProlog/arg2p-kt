modifyArgumentationGraph :-
    findall(([Rules, Top, [bp, Checked], G, I], [Z, P, Y, GG, II]), (
        context_check(argument([Rules, Top, [bp, Checked], G, I])),
        member(Y, Checked),
        context_check(argument([Z, P, Y, GG, II]))
    ), TemplateBpPairs),
    checkBpArguments(TemplateBpPairs).


checkBpArguments([]).
checkBpArguments(TemplateBpPairs) :-
    createBpArguments(TemplateBpPairs, BpPairs),
    createBpAttacks(BpPairs).


% Bp Argument Creation

createBpArguments([], []).
createBpArguments([(Original, [Z, P, Y, B, I])|Others], [(Conflict, [Z, P, Y, B, I])|BpPairs]) :-
    Conflict = [[artificial|Z], artificial, [neg, burdmet(Y)], [], [[artificial], [artificial], []]],
    context_assert(argument(Conflict)),
    context_assert(support(Original, Conflict)),
    liftBpAttacks(Original, Conflict),
    createBpArguments(Others, BpPairs).

liftBpAttacks(Template, BpArg) :-
    findall(_, (
        context_check(attack(T, A, Template, O)),
        context_assert(attack(T, A, BpArg, O))
    ), _).


% Bp Arguments Evaluation

createBpAttacks(BpPairs) :-
    generateBpsEvaluationChain(BpPairs, OrderedBpPairs),
    evaluateBurdenedArgs(OrderedBpPairs).

% The function provide an evaluation order for the bp pairs given in input.
% The ordering is based on the attack relation between the nodes
%   If it is possible to reach a node A from a node B using attacks, then B should be evaluated before A
%   The algorithm does not admit cycles in the graph

generateBpsEvaluationChain([], []).
generateBpsEvaluationChain([BpPair|BpPairs], OrderedBpPairs) :-
    generateBpsEvaluationChain(BpPairs, TempOrderedBpPairs),
    insertBpPair(BpPair, TempOrderedBpPairs, OrderedBpPairs).

insertBpPair(BpPair, [], [BpPair]).
insertBpPair((Bp, Burdened), [(BpL, BurdenedL)|Others], [(BpL, BurdenedL)|Return]) :-
    bp_grounded::argumentChain(BurdenedL, Burdened),
    insertBpPair((Bp, Burdened), Others, Return), !.
insertBpPair((Bp, Burdened), [(BpL, BurdenedL)|Others], [(Bp, Burdened)|[(BpL, BurdenedL)|Others]]).

% BpPairs Evaluation

evaluateBurdenedArgs([]).
evaluateBurdenedArgs([(Bp, Burdened)|Others]) :-
    context_active(X),
    grounded:::argumentLabelling,
    statusToAttack((Bp, Burdened), Attack),
    context_checkout(X),
    context_assert(Attack),
    evaluateBurdenedArgs(Others).

statusToAttack((Bp, Burdened), attack(bprebut, Burdened, Bp, Bp)) :-
    context_check(in(Burdened)).
statusToAttack((Bp, Burdened), attack(bprebut, Bp, Burdened, Burdened)) :-
    \+ context_check(in(Burdened)).
