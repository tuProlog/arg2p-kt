computeGlobalAcceptance([SORTEDSTATIN, SORTEDSTATOUT, SORTEDSTATUND], [ARGSIN, ARGSOUT, ARGSUND]) :-
    buildGraph([Arguments, Attacks, Supports]),!,
    buildArgumentLabelling([Arguments, Attacks, Supports], [ARGSIN, ARGSOUT, ARGSUND]),
    buildStatementLabelling([ARGSIN, ARGSOUT, ARGSUND], [STATIN, STATOUT, STATUND]),
    sort(STATIN, SORTEDSTATIN),
    sort(STATOUT, SORTEDSTATOUT),
    sort(STATUND, SORTEDSTATUND),
    storeResults(ARGSIN, ARGSOUT, ARGSUND).

buildGraph([Arguments, Attacks, Supports]) :-
    graphBuildMode(base),
    buildArgumentationGraph([Arguments, Attacks, Supports]).

buildGraph([Arguments, Attacks, Supports]) :-
    graphBuildMode(pgraph),
    buildPrefArgumentationGraph([Arguments, Attacks, Supports]).

buildArgumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]) :-
    argumentLabellingMode(grounded),
    argumentGroundedLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]).

buildArgumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]) :-
    argumentLabellingMode(complete),
    argumentCompleteLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]).

buildArgumentLabelling([Arguments, Attacks, Supports], [BPIN, BPOUT, BPUND]) :-
    argumentLabellingMode(bp_grounded_partial),
    argumentGroundedLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]),
    argumentBPLabelling(partial, [IN, OUT, UND], [BPIN, BPOUT, BPUND]).

buildArgumentLabelling([Arguments, Attacks, Supports], [BPIN, BPOUT, BPUND]) :-
    argumentLabellingMode(bp_grounded_complete),
    argumentGroundedLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]),
    argumentBPLabelling(complete, [IN, OUT, UND], [BPIN, BPOUT, BPUND]).

buildStatementLabelling([ARGSIN, ARGSOUT, ARGSUND], [IN, OUT, UND]) :-
    statementLabellingMode(base),
    statementLabelling([ARGSIN, ARGSOUT, ARGSUND], [IN, OUT, UND]).

storeResults(ARGSIN, ARGSOUT, ARGSUND) :-
    retractall(argsLabelling(_, _, _)),
    asserta(argsLabelling(ARGSIN, ARGSOUT, ARGSUND)).
