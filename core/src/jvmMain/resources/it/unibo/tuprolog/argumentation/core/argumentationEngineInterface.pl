argTuProlog.

buildLabelSets([StatIn, StatOut, StatUnd], [ArgsIn, ArgsOut, ArgsUnd]) :-
    context_reset,
    parser:::convertAllRules(ArgRules),
    debug::printTheory(ArgRules),
    abstract::computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]),
    utils::store(graph(_), graph([Arguments, Attacks, Supports])),
    utils::store(labelling(_), labelling([ArgsIn, ArgsOut, ArgsUnd])),
    debug::printArgumentationGraph(Arguments, Attacks, Supports),
    debug::printArgumentLabelling([ArgsIn, ArgsOut, ArgsUnd]),
    debug::printStatementLabelling([StatIn, StatOut, StatUnd]).

buildLabelSets(StatIn, StatOut, StatUnd) :-
    buildLabelSets([StatIn, StatOut, StatUnd], _).

buildLabelSets :-
    buildLabelSets(_, _).

answerQuery(Goal, Yes, No, Und) :-
    parser::convertAllRules(ArgRules),
    debug::printTheory(ArgRules),
    structured::computeStatementAcceptance(ArgRules, Goal, Yes, No, Und).
