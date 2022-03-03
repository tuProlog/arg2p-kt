argTuProlog.

buildLabelSets([StatIn, StatOut, StatUnd], [ArgsIn, ArgsOut, ArgsUnd]) :-
    context_reset,
    parser:::convertAllRules(_),
    abstract::computeGlobalAcceptance(_, [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]).

buildLabelSets(StatIn, StatOut, StatUnd) :-
    buildLabelSets([StatIn, StatOut, StatUnd], _).

buildLabelSets :-
    context_reset,
    parser:::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]),
    debug::printArgumentationGraph(Arguments, Attacks, Supports),
    debug::printArgumentLabelling([ArgsIn, ArgsOut, ArgsUnd]),
    debug::printStatementLabelling([StatIn, StatOut, StatUnd]).

answerQuery(Goal, Yes, No, Und) :-
    context_reset,
    parser:::convertAllRules(_),
    structured:::computeStatementAcceptance(Goal, Yes, No, Und).

answerQuery(Goal, Res) :-
    context_reset,
    parser:::convertAllRules(_),
    structured:::query(Goal, Res).


buildLabelSetsSilent :-
    context_reset,
    parser:::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance.