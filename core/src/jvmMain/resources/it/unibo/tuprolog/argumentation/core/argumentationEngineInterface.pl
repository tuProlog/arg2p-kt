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
    computeStatementAcceptance(Goal, Yes, No, Und).

answerQuery(Goal, Res) :-
    context_reset,
    parser:::convertAllRules(_),
    structured:::query(Goal, Res).

buildLabelSetsSilent :-
    context_reset,
    parser:::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance.

%==============================================================================
% TO MOVE
%==============================================================================

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    \+ queryMode,
    abstract:::computeGlobalAcceptance,
    mineResults(Goal, YesResult, NoResult, UndResult).

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    queryMode,
    findall(_, buildArgument(Goal, _), _),
    graphBuildMode(X),
    X:::buildAttacks,
    abstract:::modifyGraph,
    abstract:::buildArgumentLabelling,
    abstract:::buildStatementLabelling,
    mineResults(Goal, YesResult, NoResult, UndResult).

mineResults(Goal, YesResult, NoResult, UndResult) :-
    findall(Goal, context_check(statIn([Goal])), In),
    findall(Goal, context_check(statOut([Goal])), Out),
    findall(Goal, context_check(statUnd([Goal])), Und),
    utils::sort(In, YesResult),
    utils::sort(Out, NoResult),
    utils::sort(Und, UndResult).

%==============================================================================
% ARGUMENT CONSTRUCTION
%==============================================================================

ground_vars(Term, Grounded) :-
    copy_term(Term, Copy),
    ground_vars(Copy, Grounded, 0, _).

ground_vars(Var, var, N0, N) :-
    var(Var), !,
    N is N0 + 1.

ground_vars(Term, Grounded, N0, N) :-
    compound(Term), !,
    Term =.. [F|Args],
    ground_vars_list(Args, GArgs, N0, N),
    Grounded =.. [F|GArgs].

ground_vars(Term, Term, N, N) :-
    atomic(Term).

ground_vars_list([], [], N, N).
ground_vars_list([X|Xs], [GX|GXs], N0, N) :-
    ground_vars(X, GX, N0, N1),
    ground_vars_list(Xs, GXs, N1, N).

buildArgument(Query, Argument) :-
    % write("Calling on "),write(Query),nl,
    ground_vars(Query, Normalized),
    \+ context_check(explored(Normalized)),
    findall(_, (
        buildSingleArgument([Query], Argument, Support),
        % write("Backtracking: "),write(Query),nl,
        saveArgument(query, [Query], Argument),
        saveSupports(Argument, Support),
        attack::findPossibleAttackers(Argument, [Conflict]),
        buildArgument(Conflict, _)
    ), _),
    context_assert(explored(Query)),
    fail.
buildArgument(Query, Argument) :- context_check(clause(conc([Query]), argument(Argument))).


buildSingleArgument(Query, Argument, Support) :-
    build(Query, [], Groundings, [AllRules, TopRule, LastDefRules, DefRules, DefPremises], Support),
    utils::deduplicate(DefRules, CDefRules),
    utils::deduplicate(DefPremises, CDefPremises),
    utils::deduplicate(Groundings, CGroundings),
    Argument = [AllRules, TopRule, Query, CGroundings, [LastDefRules, CDefRules, CDefPremises]].

% Premise

build(Conclusion, _, [Conclusion], [AllRules, TopRule, LastDefRules, DefRules, DefPremises], []) :-
    parser::premise(Id, Conclusion),
    premiseRules(Id, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]).

% Rule

build(Conclusion, Ids, [Conclusion|Conclusions], [AllRules, TopRule, LastDefRules, DefRules, DefPremises], Support) :-
    parser::rule(Id, Premises, Conclusion),
    %\+ member(Id, Ids),
    buildPremises(Premises, [Id|Ids], Conclusions, ResRules, Support),
    % write("premises ok"),nl,
    ruleRules(Id, ResRules, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]).

buildPremises([], _, [], [[], [], [], []], []).
buildPremises([H|T], Ids, TConclusions, TRules, TSupport) :-
    % write("Prolog: "),write(H),nl,
    parser::prolog([H], Term), !,
    % write("It was prolog!"),nl,
    buildPremises(T, Ids, TConclusions, TRules, TSupport),
    (callable(Term) -> call(Term); Term).
buildPremises([H|T], [Unless|TConclusions], TRules, TSupport) :-
    parser::contrary([Unless], _), !,
    buildPremises(T, Ids, TConclusions, TRules, TSupport).
buildPremises([H|T], Ids, Conclusions, Rules, [[AllRules, P, PP, HConclusions, [LastDefRules, DefRules, DefPremises]]|TSupport]) :-
    % build([H], Ids, HConclusions, HRules),
    buildPremises(T, Ids, TConclusions, TRules, TSupport),
    buildArgument(H, [AllRules, P, PP, HConclusions, [LastDefRules, DefRules, DefPremises]]),
    % write("Premise ok"),nl,
    utils::appendLists([HConclusions, TConclusions], Conclusions),
    mergeRules([AllRules, [], LastDefRules, DefRules, DefPremises], TRules, Rules).


premiseRules(Id, [[Id], none, [], [], [Id]]) :- \+ parser::check_strict(Id), !.
premiseRules(Id, [[Id], none, [], [], []]) :- parser::check_strict(Id).

ruleRules(Id, [AllRules, _, DefRules, DefPremises],
    [[Id|AllRules], Id, [Id], [Id|DefRules], DefPremises]) :- \+ parser::check_strict(Id), !.
ruleRules(Id, [AllRules, LastDefRules, DefRules, DefPremises],
    [[Id|AllRules], Id, LastDefRules, DefRules, DefPremises]) :- parser::check_strict(Id).

mergeRules([], [AllRules, LastDefRules, DefRules, DefPremises], [AllRules, LastDefRules, DefRules, DefPremises]) :- !.
mergeRules([HAR, _, HLDR, HDR, HDP], [TAR, TLDR, TDR, TDP], [AR, LDR, DR, DP]) :-
   utils::appendLists([HAR, TAR], AR),
   utils::appendLists([HLDR, TLDR], LDR),
   utils::appendLists([HDR, TDR], DR),
   utils::appendLists([HDP, TDP], DP).


%--------------------------------------------------------------------------
% Save stuff
%--------------------------------------------------------------------------

saveArgument(T, Conclusion, Argument) :-
    \+ context_check(clause(conc(Conclusion), argument(Argument))),
    % write("Saving Argument: "),
    % write(Argument),nl,
    context_assert(newConc(T, Conclusion)),
    utils::hash(argument(Argument), Id),
    context_assert(argument(Argument)),
    context_assert(conc(Conclusion) :- argument(Argument)),
    context_assert(arg(Id) :- argument(Argument)).

saveSupports(_, []).
saveSupports(A, [B|T]) :-
    saveSupport(B, A),
    saveSupports(A, T).

saveSupport(A, B) :-
    \+ context_check(support(A, B)),
    % write("Saving Support: "),
    % write(A),write(" -> "),write(B),nl,
    context_assert(support(A, B)).
