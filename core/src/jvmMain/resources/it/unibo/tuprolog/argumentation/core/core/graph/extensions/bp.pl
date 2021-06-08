modifyArgumentationGraph(bp, _, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    generateBp([Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]).

generateBp([Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    findall(([Rules, Top, [bp, Checked]], [Z, P, Y]), (
        member([Rules, Top, [bp, Checked]], Arguments),
        member(Y, Checked),
        member([Z, P, Y], Arguments)
    ), Res),
    once(checkBpArguments(Res, Arguments, Attacks, Supports, NewArguments, NewAttacks, NewSupports)).

checkBpArguments([], Arguments, Attacks, Supports, Arguments, Attacks, Supports).
checkBpArguments(Res, Arguments, Attacks, Supports, NewArguments, FinalAttacks, NewSupports) :-
    createBpArguments(Res, Arguments, Attacks, Supports, BpArguments, NewArguments, NewAttacks, NewSupports),
    createBpAttacks(BpArguments, NewArguments, NewAttacks, NewSupports, FinalAttacks).

createBpArguments([], Arguments, Attacks, Supports, [], Arguments, Attacks, Supports).
createBpArguments([(Original, [Z, P, Y])|Others], Arguments, Attacks, Supports, [(Conflict,[Z, P, Y])|BpArguments], [Conflict|NewArguments], NewAttacks, [(Original, Conflict)|NewSupports]) :-
    Conflict = [[artificial|Z], artificial, [neg, burdmet(Y)]],
    asserta(argument(Conflict)),
    asserta(support(Original, Conflict)),
    liftBpAttacks(Original, Conflict, Attacks, LiftedAttacks),
    createBpArguments(Others, Arguments, LiftedAttacks, Supports, BpArguments, NewArguments, NewAttacks, NewSupports).

liftBpAttacks(Original, BpArg, Attacks, NewAttacks) :-
    findall((T, A, BpArg), (
        member((T, A, Original), Attacks),
        asserta(attack(T, A, BpArg)),
        attack(T, A, Original, C),
        asserta(attack(T, A, BpArg, C))
    ), LiftedAttacks),
    append(Attacks, LiftedAttacks, NewAttacks).

createBpAttacks(BpArguments, Arguments, Attacks, Supports, NewAttacks) :-
    generateBpsEvaluationChain(BpArguments, Attacks, OrderedBpArguments),
    evaluateBurdenedArgs(OrderedBpArguments, Arguments, Attacks, Supports, NewAttacks).

evaluateBurdenedArgs([], _, Attacks, _, Attacks).
evaluateBurdenedArgs([(Bp, Burdened)|Others], Arguments, Attacks, Supports, FinalAttacks) :-
    argumentGroundedLabelling([Arguments, Attacks, Supports], [In, Out, Und]),
    statusToAttack((Bp, Burdened), [In, Out, Und], NewAttack),
    evaluateBurdenedArgs(Others, Arguments, [NewAttack|Attacks], Supports, FinalAttacks).

statusToAttack((Bp, Burdened), [In, Out, Und], (bprebut, Burdened, Bp)) :-
    member(Burdened, In),
    asserta(attack(bprebut, Burdened, Bp)),
    asserta(attack(bprebut, Burdened, Bp, Bp)).
statusToAttack((Bp, Burdened), [In, Out, Und], (bprebut, Bp, Burdened)) :-
    \+ member(Burdened, In),
    asserta(attack(bprebut, Bp, Burdened)),
    asserta(attack(bprebut, Bp, Burdened, Burdened)).

% Devo dare un ordine di valutazione
%   Per ogni membro della lista valuto se è un mio predecessore, al primo che non lo è lo inserisco prima
generateBpsEvaluationChain([], Attacks, []).
generateBpsEvaluationChain([Arg|Others], Attacks, OrderedBpArguments) :-
    generateBpsEvaluationChain(Others, Attacks, Old),
    insertBpArg(Arg, Attacks, Old, OrderedBpArguments).

insertBpArg(Arg, Attacks, [], [Arg]).
insertBpArg((Bp, Burdened), Attacks, [(BpL, BurdenedL)|Others], [(Bp, Burdened)|[(BpL, BurdenedL)|Others]]) :-
    \+ argumentChain(BurdenedL, Burdened, Attacks).
insertBpArg((Bp, Burdened), Attacks, [(BpL, BurdenedL)|Others], [(BpL, BurdenedL)|Return]) :-
    argumentChain(BurdenedL, Burdened, Attacks),
    insertBpArg((Bp, Burdened), Attacks, Others, Return).

conflict([bp, Atom], [neg, bp, Atom]).
conflict([neg, bp, Atom], [bp, Atom]).
