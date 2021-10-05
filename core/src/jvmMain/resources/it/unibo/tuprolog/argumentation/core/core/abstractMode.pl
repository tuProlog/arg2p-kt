computeGlobalAcceptance(Rules, [NewArguments, NewAttacks, NewSupports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]) :-
    buildGraph(Rules, [Arguments, Attacks, Supports]),
    modifyGraph(Rules, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]),
    buildArgumentLabelling(Rules, [NewArguments, NewAttacks, NewSupports], [ArgsIn, ArgsOut, ArgsUnd]),
    buildStatementLabelling([ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]).

buildGraph(Rules, [Arguments, Attacks, Supports]) :-
    graphBuildMode(X),
    X::buildArgumentationGraph(Rules, [Arguments, Attacks, Supports]).

modifyGraph(Rules, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]) :-
    findall(X, graphExtension(X), Ext),
    modifyGraph(Rules, Ext, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]).

modifyGraph(_, [], [Arguments, Attacks, Supports], [Arguments, Attacks, Supports]).
modifyGraph(Rules, [X|Ext], [Arguments, Attacks, Supports], [UnionArguments, UnionAttacks, UnionSupports]) :-
    modifyGraph(Rules, Ext, [Arguments, Attacks, Supports], [NewArguments, NewAttacks, NewSupports]),
    X::modifyArgumentationGraph(Rules, [NewArguments, NewAttacks, NewSupports], [UnionArguments, UnionAttacks, UnionSupports]).

buildArgumentLabelling(Rules, [Arguments, Attacks, Supports], [In, Out, Und]) :-
    argumentLabellingMode(X),
    X::argumentLabelling(Rules, [Arguments, Attacks, Supports], [In, Out, Und]).

buildStatementLabelling([ArgsIn, ArgsOut, ArgsUnd], [In, Out, Und]) :-
    statementLabellingMode(X),
    X::statementLabelling([ArgsIn, ArgsOut, ArgsUnd], [In, Out, Und]).

