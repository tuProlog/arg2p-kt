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
