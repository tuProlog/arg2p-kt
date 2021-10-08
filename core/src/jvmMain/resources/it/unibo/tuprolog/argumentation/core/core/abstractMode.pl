computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]) :-
    buildGraph,
%    modifyGraph,
    buildArgumentLabelling,
    buildStatementLabelling(StatIn, StatOut, StatUnd),
    recoverGraph(Arguments, Attacks, Supports),
    recoverLabelling(ArgsIn, ArgsOut, ArgsUnd).

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

buildStatementLabelling(StatIn, StatOut, StatUnd) :-
    statementLabellingMode(X),
    X::statementLabelling(StatIn, StatOut, StatUnd).

recoverGraph(Args, Attacks, Supports) :-
        findall(X, cache_check(argument(X)), TempArgs),
        findall((T, A, B, C), cache_check(attack(T, A, B, C)), TempAttacks),
        findall((A, B), cache_check(support(A, B)), TempSupports),
        utils::sort(TempArgs, Args),
        utils::sort(TempAttacks, Attacks),
        utils::sort(TempSupports, Supports).

recoverLabelling(ArgsIn, ArgsOut, ArgsUnd) :-
        findall(X, cache_check(in(X)), TempArgsIn),
        findall(X, cache_check(out(X)), TempArgsOut),
        findall(X, cache_check(und(X)), TempArgsUnd),
        utils::sort(TempArgsIn, ArgsIn),
        utils::sort(TempArgsOut, ArgsOut),
        utils::sort(TempArgsUnd, ArgsUnd).