%! argTuProlog
%  The solver is an arg2p one.
argTuProlog.

%! solve
%  The labelling of the theory, in the context: one solution for each labelling.
solve :-
    context_reset,
    structuredWhole::computeGlobalAcceptance.

%! solve(-StatIn, -StatOut, -StatUnd)
%  The statements of the theory labelled IN, OUT and UND, one solution for each labelling.
solve(StatIn, StatOut, StatUnd) :-
    context_reset,
    structuredWhole::computeStatementLabels(StatIn, StatOut, StatUnd).

%! solve(+Query)
%  Query is a statement labelled IN (as answerMode states, see solve/4).
solve(Query) :-
    context_reset,
    statementAcceptance:::accepted(Query).

%! solve(+Query, -Result)
%  The labels of the statements unifying with Query, as in(S), out(S) and und(S) terms.
solve(Query, Result) :-
    context_reset,
    statementAcceptance:::computeStatementAcceptance(Query, Result).

%! solve(+Query, -In, -Out, -Und)
%  The statements unifying with Query labelled IN, OUT and UND: with queryMode only the part of the graph they depend on
%  is built. With several labellings, answerMode states how: one solution for each (enumerate), or one across them all
%  (credulous, skeptical).
solve(Query, In, Out, Und) :-
    context_reset,
    statementAcceptance:::computeStatementAcceptance(Query, In, Out, Und).

%==============================================================================
% The entry points of earlier versions, kept for the theories and the programs written for them: solve/0..4 in their
% place
%==============================================================================

%! buildLabelSets(-StatIn, -StatOut, -StatUnd)
%  solve/3.
buildLabelSets(StatIn, StatOut, StatUnd) :- solve(StatIn, StatOut, StatUnd).

%! buildLabelSets(-StatementLabels, -ArgumentLabels)
%  The statements and the arguments of the theory labelled IN, OUT and UND, as [In, Out, Und] lists: solve/3, the
%  arguments as store::argumentLabelling/3 reads them.
buildLabelSets([StatIn, StatOut, StatUnd], [ArgsIn, ArgsOut, ArgsUnd]) :-
    solve(StatIn, StatOut, StatUnd),
    store::argumentLabelling(ArgsIn, ArgsOut, ArgsUnd).

%! buildLabelSets
%  solve/0, the graph and its labels printed (see the module graphPrinter).
buildLabelSets :-
    solve,
    store::graph(Arguments, Attacks, Supports),
    store::argumentLabelling(ArgsIn, ArgsOut, ArgsUnd),
    store::statementLabelling(StatIn, StatOut, StatUnd),
    graphPrinter::printArgumentationGraph(Arguments, Attacks, Supports),
    graphPrinter::printArgumentLabelling([ArgsIn, ArgsOut, ArgsUnd]),
    graphPrinter::printStatementLabelling([StatIn, StatOut, StatUnd]).

%! buildLabelSetsSilent
%  solve/0.
buildLabelSetsSilent :- solve.

%! answerQuery(+Goal, -Yes, -No, -Und)
%  solve/4.
answerQuery(Goal, Yes, No, Und) :- solve(Goal, Yes, No, Und).

%! answerQuery(+Goal, -Result)
%  solve/2.
answerQuery(Goal, Res) :- solve(Goal, Res).

%! answerQuery(+Goal)
%  solve/1.
answerQuery(Goal) :- solve(Goal).
