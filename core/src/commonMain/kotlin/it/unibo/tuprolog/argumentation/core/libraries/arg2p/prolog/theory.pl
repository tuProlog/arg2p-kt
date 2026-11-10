%! argTuProlog
%  The solver is an arg2p one.
argTuProlog.

%! solve
%  The labelling of the theory, in the context: one solution for each labelling.
solve :-
    context_reset,
    pipeline::evaluate.

%! solve(-StatIn, -StatOut, -StatUnd)
%  The statements of the theory labelled IN, OUT and UND, one solution for each labelling.
solve(StatIn, StatOut, StatUnd) :-
    context_reset,
    pipeline::statementLabels(StatIn, StatOut, StatUnd).

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
%  solve/0, the graph and its labels printed (see printArgumentationGraph/3).
buildLabelSets :-
    solve,
    store::graph(Arguments, Attacks, Supports),
    store::argumentLabelling(ArgsIn, ArgsOut, ArgsUnd),
    store::statementLabelling(StatIn, StatOut, StatUnd),
    printArgumentationGraph(Arguments, Attacks, Supports),
    printArgumentLabelling([ArgsIn, ArgsOut, ArgsUnd]),
    printStatementLabelling([StatIn, StatOut, StatUnd]).

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

%==============================================================================
% THE GRAPH AND ITS LABELS PRINTED (for buildLabelSets/0)
%==============================================================================

%! printArgumentationGraph(+Arguments, +Attacks, +Supports)
%  The graph printed, its arguments part by part.
printArgumentationGraph(Arguments, Attacks, Supports) :-
    findall(
        [Rules, '\n',  ' TOPRULE ',  TopRule, '\n', ' CONCLUSION ', Conclusion, '\n', ' BODY ', Body, '\n', ' INFO ', Info, '\n'],
        (member(Argument, Arguments), store::newArgument(Rules, TopRule, Conclusion, Body, Info, Argument)),
        ArgumentsToPrint
    ),
    findall((A1, ' SUPPORTS ', A2), member((A1, A2), Supports), SupportsToPrint),
    findall((A1, ' ', T, ' ', A2, ' ON ', A3),  member((T, A1, A2, A3), Attacks),  AttacksToPrint),
    write('HERE THE ARGUMENTATION GRAPH'),nl,
    writeListNl(ArgumentsToPrint),
    writeListNl(SupportsToPrint),
    writeListNl(AttacksToPrint).


%! printArgumentLabelling(+Labels)
%  The arguments of [In, Out, Und] printed by label.
printArgumentLabelling([IN, OUT, UND]) :-
    write('HERE THE ARGUMENTS LABELLED IN: '),nl,
    writeListNl(IN),
    write('HERE THE ARGUMENTS LABELLED OUT: '),nl,
    writeListNl(OUT),
    write('HERE THE ARGUMENTS LABELLED UND: '),nl,
    writeListNl(UND).


%! printStatementLabelling(+Labels)
%  The statements of [In, Out, Und] printed by label.
printStatementLabelling([In, Ni, Und]) :-
    write('HERE THE STATEMENTS LABELLED IN: '),nl,
    writeListNl(In),
    write('HERE THE STATEMENTS LABELLED OUT: '),nl,
    writeListNl(Ni),
    write('HERE THE STATEMENTS LABELLED UND: '),nl,
    writeListNl(Und).

% writeListNl(+List): the items of List written each on a line
writeListNl([]).
writeListNl([X|Others]) :-
    write(X), nl,
    writeListNl(Others).
