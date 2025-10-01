computeGlobalAcceptance :-
    buildGraph,
    modifyGraph,
    buildArgumentLabelling,
    buildStatementLabelling.

computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]) :-
    computeGlobalAcceptance,
    utils::recoverGraph(Arguments, Attacks, Supports),
    utils::recoverArgumentLabelling(ArgsIn, ArgsOut, ArgsUnd),
    utils::recoverStatementLabelling(StatIn, StatOut, StatUnd).

buildGraph :-
    graphBuildMode(X),
    X:::buildArgumentationGraph.

modifyGraph :-
    findall(X, graphExtension(X), Ext),
    modifyGraph(Ext).

modifyGraph([]).
modifyGraph([X|Ext]) :-
    modifyGraph(Ext),
    X:::modifyArgumentationGraph.

buildArgumentLabelling :-
    argumentLabellingMode(X),
    X:::argumentLabelling.

buildStatementLabelling :-
    statementLabellingMode(X),
    X:::statementLabelling.

solve(Arguments, Attacks, I, O, U) :-
    solve(Arguments, Attacks),
    utils::recoverArgumentLabellingAbstract(I, O, U).

solve(Arguments, Attacks) :-
    context_reset,
    findall(_, (
        member(A, Arguments),
        standard_af::saveArgument(graph, [A], [[A], A, [A], [], [[], [], []]])
    ), _),
    findall(_, (
        member((A, B), Attacks),
        AA = [[A], A, [A], [], [[], [], []]],
        BB = [[B], B, [B], [], [[], [], []]],
        standard_af::saveAttack(abstract, AA, BB, BB)
    ), _),
    buildArgumentLabelling.
