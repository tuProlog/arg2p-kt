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

% The lists are visited natively (for_each): as the goal of a findall, every step of the saving would visit them
solve(Arguments, Attacks) :-
    context_reset,
    utils::for_each(A, Arguments,
        standard_af::saveArgument(graph, [A], [[A], A, [A], [], [[], [], []]])
    ),
    utils::for_each((X, Y), Attacks, (
        XX = [[X], X, [X], [], [[], [], []]],
        YY = [[Y], Y, [Y], [], [[], [], []]],
        standard_af::saveAttack(abstract, XX, YY, YY)
    )),
    buildArgumentLabelling.
