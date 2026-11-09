%! computeGlobalAcceptance
%  The graph of the theory, changed by the steps the flags select, and its argument and statement labellings, in
%  the context.
computeGlobalAcceptance :-
    buildGraph,
    phases::modifyGraph,
    phases::buildArgumentLabelling,
    phases::buildStatementLabelling.

%! computeStatementLabels(-In, -Out, -Und)
%  As computeGlobalAcceptance/0, with the labels of the statements, one solution per labelling.
computeStatementLabels(StatIn, StatOut, StatUnd) :-
    computeGlobalAcceptance,
    store::statementLabelling(StatIn, StatOut, StatUnd).

%! buildGraph
%  The graph of the theory, by the builder of its profile (graphBuilder), once the theory is checked against it:
%  with the solvers of a session building it together, the graph of all of them (see store::graph_begin/1,
%  store::graph_settle/0).
buildGraph :-
    phases::checkProfile,
    store::graph_begin(whole),
    phases::builder(X),
    phases::dispatched(X, buildArgumentationGraph/0),
    X:::buildArgumentationGraph,
    store::graph_settle.
