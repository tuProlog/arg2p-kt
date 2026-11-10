%! buildArgumentationGraph
%  The arguments of the theory and the attacks between them.
buildArgumentationGraph :-
    buildArguments,
    buildAttacks.

%! buildArguments
%  The arguments of the theory.
buildArguments :-
    argumentBuilder::buildArguments.

%! buildQueryGraph(+Query, +Locality, +Steps)
%  The part of the graph the labels of the arguments for Query depend on, as Locality goes (directional, connected): the
%  arguments explored from Query, and as Steps relate statements (see queryGraph::explore/3), then the attacks between
%  them. With the solvers of a session exploring it together (see GraphSharing), what the others explore and build carried
%  on with (see sharedExploration/2).
buildQueryGraph(Query, Locality, Steps) :-
    queryGraph::explore(Query, Locality, Steps),
    sharedExploration(Locality, Steps),
    buildAttacks.

%! buildAttacks
%  The attacks between the saved arguments: all of them, or, in query mode, those explored (see buildQueryGraph/3).
buildAttacks :-
    attackBuilder::buildAttacks.

% sharedExploration(+Locality, +Steps): the exploration carried on with what the other solvers of a session explore and
% build, until none has more (see store::graph_explore/2); nothing for a solver alone. The statements they
% explored are explored here too, with the rules here, and those whose rules use the conclusions of the arguments
% they built again (see queryGraph::resume/1): what an argument makes worth exploring is explored by its
% builder (see queryGraph::dependency/3 and queryGraph::extend/2)
sharedExploration(Locality, Steps) :-
    store::graph_explore(Statements, Claims), !,
    queryGraph::exploreAll(Statements),
    queryGraph::resume(Claims),
    queryGraph::expand(Locality),
    queryGraph::extend(Steps, Locality),
    sharedExploration(Locality, Steps).
sharedExploration(_, _).
