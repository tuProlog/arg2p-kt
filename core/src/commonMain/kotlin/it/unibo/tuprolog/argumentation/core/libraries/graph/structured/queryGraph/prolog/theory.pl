%! explore(+Query)
%  The arguments for Query and those their labels may depend on (see exploreStatement/2). With cycles among the
%  rules, a statement needed as a premise while its arguments are being built only gets those built so far: the
%  exploration is then repeated, from scratch, until it builds no new argument (the arguments are finitely many,
%  as none uses a rule twice).
explore(Query) :-
    store::forgetNewConclusions(query),
    context_retract(cycle),
    exploreStatement(Query, dependency),
    (context_check(cycle), store::newConclusion(query, _) ->
        context_retract(explored(_)),
        explore(Query)
    ;
        true
    ).

%! explore(+Query, +Locality, +Steps)
%  The part of the graph the labels of the arguments for Query depend on, as Locality goes (see
%  pipeline::locality/1): the arguments for Query and those they depend on (explore/1), those depending
%  on them for the connected locality (expand/1), then what Steps, the steps changing the graph that apply, relate to
%  them (extend/2).
explore(Query, Locality, Steps) :-
    explore(Query),
    expand(Locality),
    extend(Steps, Locality).

%! resume(+Statements)
%  The exploration carried on once arguments for Statements, built by another solver of a session, are saved: the
%  statements explored whose rules use them as premises, directly or through other statements, explored again, the
%  others left as they are.
resume(Statements) :-
    builtOn(Statements, [], Affected),
    findall(_, (member(S, Affected), context_retract(explored(S))), _),
    findall(_, (member(S, Affected), explore(S)), _).

% builtOn(+Statements, +Seen, -Affected): the statements explored whose rules use one of Statements as a premise,
% as they were explored (q(X), rather than the instance q(1) of the rule using p(1)), and in turn those whose
% rules use them; Seen are those found so far (with groundVars/2)
builtOn([], _, []).
builtOn([Statement|Statements], Seen, Affected) :-
    findall(S, (store::ruleUsing(Statement, C), exploredAs(C, S)), Found),
    unseen(Found, Seen, New, Seen1),
    utils::appendLists([Statements, New], Next),
    builtOn(Next, Seen1, Rest),
    utils::appendLists([New, Rest], Affected).

% exploredAs(+Statement, -Explored): Explored is explored and unifies with Statement, as it was explored
exploredAs(Statement, Explored) :-
    context_matching(explored, Statement, explored(Explored)).

unseen([], Seen, [], Seen).
unseen([S|Ss], Seen, New, Seen1) :-
    groundVars(S, N),
    (member(N, Seen) ->
        unseen(Ss, Seen, New, Seen1)
    ;
        New = [S|New1],
        unseen(Ss, [N|Seen], New1, Seen1)
    ).

%! explored(?Statement)
%  Statement is explored (with the statements it generalises).
explored(Statement) :-
    context_check(explored(Statement)).

%! exploreAll(+Queries)
%  Each of Queries explored (see explore/1), the statements the other solvers of a session explored: one naming a rule
%  of this solver with its conclusion as well, as an argument attacking the rule (a conflict with it, an undercut) makes
%  a solver explore what the rule concludes, which only the solver with the rule knows (see
%  attackSearch::possiblyAttacked/2).
exploreAll(Queries) :-
    findall(_, (member(Query, Queries), explore(Query)), _),
    findall(_, (member(Rule, Queries), store::ruleConclusion(Rule, Conclusion), explore(Conclusion)), _).

%! buildSubGraph(+Query, ?Argument)
%  Argument is each argument for Query, a premise, once explored.
buildSubGraph(Query, Argument) :-
    exploreStatement(Query, premise),
    store::argumentFor([Query], Argument).

% exploreStatement(+Query, +Use): builds the arguments for Query, and those their labels may depend on (see
% dependency/3), recursively, unless it is explored (once, or once more for each round of explore/1):
% explored(Query) covers the statements Query generalises. A statement met again while it is being explored only
% has the arguments built so far: needed as a premise (Use), that is a cycle; needed as a dependency, its
% exploration under way builds them all.
exploreStatement(Query, Use) :-
    parser::hook(explored/2, Extension), !,
    Extension::explored(Query, Explored),
    exploreAs(Explored, Use).
exploreStatement(Query, Use) :-
    exploreAs(Query, Use).

exploreAs(Query, _) :-
    groundVars(Query, Normalized),
    context_absent(explored(Normalized)), !,
    context_assert(explored(Query)),
    context_assert(exploring(Normalized, Query)),
    findall(_, (
        argumentSearch::findSingleArgument([Query], Argument, Supports),
        recordArgument(Query, Argument, Supports),
        store::argumentId(Argument, Id),
        dependency(Id, Argument, Statement),
        exploreStatement(Statement, dependency)
    ), _),
    context_retract(exploring(Normalized, _)).
exploreAs(Query, premise) :-
    groundVars(Query, Normalized),
    context_check(exploring(_, Normalized)), !,
    (context_absent(cycle) -> context_assert(cycle) ; true).
exploreAs(_, _).

% The statements whose arguments the label of an argument may depend on, besides its sub-arguments (built with
% it): those of its possible attackers. The steps changing the graph relate it to others in turn (see extend/2).
dependency(Id, Argument, S) :- attackSearch::possibleAttackers(Id, Argument, [S]).

%! expand(+Locality)
%  For the connected locality, the arguments whose labels may depend on the ones built (see dependant/2), with
%  those they depend on in turn, until none is new.
expand(connected) :-
    findall(Id, (store::inGraph(Id), context_absent(expanded(Id))), Ids),
    Ids \== [], !,
    findall(_, (
        member(Id, Ids),
        context_assert(expanded(Id)),
        store::argumentById(Id, Argument),
        dependant(Argument, Statement),
        explore(Statement)
    ), _),
    expand(connected).
expand(_).

% The statements whose arguments may depend on an argument: those it may attack, and those of its
% super-arguments. The steps changing the graph relate others to it in turn (see extend/2).
dependant(Argument, S) :- attackSearch::possiblyAttacked(Argument, S).
dependant(Argument, S) :-
    store::claim(Argument, Claim),
    store::ruleUsing(Claim, S).

%! extend(+Steps, +Locality)
%  The exploration extended by those of Steps, the steps changing the graph, that relate statements in query mode
%  (defining queryDependencies/2 and queryDependants/2), as they extend the graph in whole mode: the statements each
%  relates to the arguments built (queryDependencies/2, and for the connected locality queryDependants/2) explored,
%  until no argument is new. When the conflicts are arguments (see conflict::argued/0), the arguments built may make more
%  of the others possible attackers or dependants: those are looked for again.
extend(Steps, Locality) :-
    findall(Step, (member(Step, Steps), relating(Step)), Relating),
    extendBy(Relating, Locality).

relating(Step) :-
    module_defines(Step, queryDependencies/2),
    module_defines(Step, queryDependants/2).

extendBy([], _) :- !.
extendBy(Steps, Locality) :-
    store::graphSize(N),
    findall(S, (store::argumentById(_, A), member(Step, Steps), related(Locality, Step, A, S)), Statements),
    findall(_, (member(S, Statements), explore(S)), _),
    store::graphSize(M),
    (N == M -> true ; again(Locality), extendBy(Steps, Locality)).

related(_, Step, Argument, S) :- Step::queryDependencies(Argument, S).
related(connected, Step, Argument, S) :- Step::queryDependants(Argument, S).

% Once arguments are added: if the conflicts are arguments, the possible attackers and the dependants of all
% of them looked for again; those of the new ones otherwise
again(Locality) :-
    conflict::argued, !,
    attackSearch::forgetPossibleAttackers,
    context_retract(expanded(_)),
    findall(_, (store::argumentById(Id, A), dependency(Id, A, S), explore(S)), _),
    expand(Locality).
again(Locality) :-
    expand(Locality).

% recordArgument(+Query, +Argument, +Supports): Argument saved with its supports, unless it already was (built in
% a previous round of explore/1, whose dependencies are explored again)
recordArgument(Query, Argument, Supports) :-
    store::saveArgument(query, [Query], Argument), !,
    store::saveSupporters(Argument, Supports).
recordArgument(_, _, _).
