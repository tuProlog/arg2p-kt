computeGlobalAcceptance(Rules, [Arguments, Attacks, Supports], [SORTEDSTATIN, SORTEDSTATOUT, SORTEDSTATUND], [ARGSIN, ARGSOUT, ARGSUND]) :-
    buildGraph(Rules, [Arguments, Attacks, Supports]), !.
    % modifyGraph(Rules, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]),
    % buildArgumentLabelling([NewArguments, NewAttacks, NewSupports], [ARGSIN, ARGSOUT, ARGSUND]),
    % buildStatementLabelling([ARGSIN, ARGSOUT, ARGSUND], [STATIN, STATOUT, STATUND]),
    % sort(STATIN, SORTEDSTATIN),
    % sort(STATOUT, SORTEDSTATOUT),
    % sort(STATUND, SORTEDSTATUND).

buildGraph(Rules, [Arguments, Attacks, Supports]) :-
    graphBuildMode(X),
    X::buildArgumentationGraph(Rules, [Arguments, Attacks, Supports]),
    debug::printArgumentationGraph(Arguments, Attacks, Supports).

modifyGraph([Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    findall(X, graphExtension(X), Ext),
    modifyGraph(Ext, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]).

modifyGraph([], [Arguments, Attacks, Supports], [Arguments, Attacks, Supports]).
modifyGraph([X|Ext], [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
    modifyGraph(Ext, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]),
    modifyArgumentationGraph(X, [NewArguments, NewAttacks, NewSupports], [UnionArguments, UnionAttacks, UnionSupports]).

buildArgumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]) :-
    argumentLabellingMode(grounded),
    argumentGroundedLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]).

buildArgumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]) :-
    argumentLabellingMode(complete),
    argumentCompleteLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]).

buildArgumentLabelling([Arguments, Attacks, Supports], [BPIN, BPOUT, BPUND]) :-
    argumentLabellingMode(bp_grounded),
    argumentBPLabelling([Arguments, Attacks, Supports], [BPIN, BPOUT, BPUND]).

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

% TODO

storeResults(ARGSIN, ARGSOUT, ARGSUND) :-
    retractall(argsLabelling(_, _, _)),
    asserta(argsLabelling(ARGSIN, ARGSOUT, ARGSUND)).
