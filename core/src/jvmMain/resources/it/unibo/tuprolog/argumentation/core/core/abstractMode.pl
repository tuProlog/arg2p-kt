computeGlobalAcceptance([[], [], []], [ArgsIn, ArgsOut, ArgsUnd], [[], [], []]) :-
    buildGraph,
%    modifyGraph,
    buildArgumentLabelling,
    findall(X, cache_check(in(X)), ArgsIn),
    findall(X, cache_check(out(X)), ArgsOut),
    findall(X, cache_check(und(X)), ArgsUnd).
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

