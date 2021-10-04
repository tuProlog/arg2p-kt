computeGlobalAcceptance(Rules, [Arguments, Attacks, Supports], [StatIn, StatOut, StatUnd], [ArgsIn, ArgsOut, ArgsUnd]) :-
    buildGraph(Rules, [Arguments, Attacks, Supports]),
    debug::printArgumentationGraph(Arguments, Attacks, Supports),
    % modifyGraph(Rules, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]),
    buildArgumentLabelling([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd]),
    debug::printArgumentLabelling([ArgsIn, ArgsOut, ArgsUnd]),
    buildStatementLabelling([ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]),
    debug::printStatementLabelling([StatIn, StatOut, StatUnd]).

buildGraph(Rules, [Arguments, Attacks, Supports]) :-
    graphBuildMode(X),
    X::buildArgumentationGraph(Rules, [Arguments, Attacks, Supports]),
    store(graph(_), graph([Arguments, Attacks, Supports])).

buildArgumentLabelling([Arguments, Attacks, Supports], [In, Out, Und]) :-
    argumentLabellingMode(X),
    X::argumentLabelling([Arguments, Attacks, Supports], [In, Out, Und]),
    store(labelling(_), labelling([In, Out, Und])).

buildStatementLabelling([ArgsIn, ArgsOut, ArgsUnd], [In, Out, Und]) :-
    statementLabellingMode(X),
    X::statementLabelling([ArgsIn, ArgsOut, ArgsUnd], [In, Out, Und]).

store(Retract, Assert) :-
    cache_retract(Retract),
    cache_assert(Assert).

% modifyGraph([Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
%    findall(X, graphExtension(X), Ext),
%    modifyGraph(Ext, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]).

% modifyGraph([], [Arguments, Attacks, Supports], [Arguments, Attacks, Supports]).
% modifyGraph([X|Ext], [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
%    modifyGraph(Ext, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]),
%    modifyArgumentationGraph(X, [NewArguments, NewAttacks, NewSupports], [UnionArguments, UnionAttacks, UnionSupports]).
