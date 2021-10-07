computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]) :-
    buildGraph.
%    modifyGraph,
%    buildArgumentLabelling,
%    buildStatementLabelling.

buildGraph :-
    graphBuildMode(X),
    X::buildArgumentationGraph.

modifyGraph :-
    findall(X, graphExtension(X), Ext),
    modifyGraph(Ext).

modifyGraph([]).
modifyGraph([X|Ext]) :-
    modifyGraph(Ext),
    X::modifyArgumentationGraph.

buildArgumentLabelling :-
    argumentLabellingMode(X),
    X::argumentLabelling.

buildStatementLabelling :-
    statementLabellingMode(X),
    X::statementLabelling.

