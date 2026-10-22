computeStatementAcceptance(Goal, Res) :-
    computeStatementAcceptance(Goal),
    mineResults(Goal, Res).

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    computeStatementAcceptance(Goal),
    mineResults(Goal, YesResult, NoResult, UndResult).

computeStatementAcceptance(Goal) :-
    \+ queryMode,
    abstract:::computeGlobalAcceptance.

computeStatementAcceptance(Goal) :-
    queryMode,
    locality(Locality),
    queryAcceptance(Locality, Goal).

% The part of the graph the labels of the arguments for the query depend on, as the semantics declares it
% (locality/1): the arguments they depend on (directional), those connected to them in either direction
% (connected), or the whole graph (whole_graph, also for a semantics that does not declare it)
locality(Locality) :-
    argumentLabellingMode(Semantics),
    Semantics::locality(L), !,
    Locality = L.
locality(whole_graph).

queryAcceptance(whole_graph, _) :- !,
    abstract:::computeGlobalAcceptance.
queryAcceptance(Locality, Goal) :-
    findall(_, buildSubGraph(Goal, _), _),
    expand(Locality),
    graphBuildMode(X),
    X:::buildAdditionalArguments,
    X:::buildAttacks,
    abstract:::modifyGraph,
    abstract:::buildArgumentLabelling,
    abstract:::buildStatementLabelling.

mineResults(Goal, YesResult, NoResult, UndResult) :-
    findall(Goal, context_check(statIn([Goal])), In),
    findall(Goal, context_check(statOut([Goal])), Out),
    findall(Goal, context_check(statUnd([Goal])), Und),
    utils::sort(In, YesResult),
    utils::sort(Out, NoResult),
    utils::sort(Und, UndResult).

mineResults(Goal, Result) :-
    findall(in(Goal), context_check(statIn([Goal])), In),
    findall(out(Goal), context_check(statOut([Goal])), Out),
    findall(und(Goal), context_check(statUnd([Goal])), Und),
    utils::appendLists([In, Out, Und], Result).

%==============================================================================
% ARGUMENT CONSTRUCTION
%==============================================================================

% buildSubGraph(+Query, ?Argument): builds the arguments for Query, and those their labels may depend on
% (see dependency/3), recursively; then Argument is each argument for Query
buildSubGraph(Query, Argument) :-
    % write("Calling on "),write(Query),nl,
    ground_vars(Query, Normalized),
    context_absent(explored(Normalized)),
    findall(_, (
        findSingleArgument([Query], Argument, Support),
        saveArgument(query, [Query], Argument),
        saveSupports(Argument, Support),
        context_id(argument(Argument), Id),
        dependency(Id, Argument, Statement),
        buildSubGraph(Statement, _)
    ), _),
    context_assert(explored(Query)),
    fail.
buildSubGraph(Query, Argument) :- context_check(clause(conc([Query]), argument(Argument))).

% The statements whose arguments the label of an argument may depend on, besides its sub-arguments (built with
% it): those that make others conflict with it (metaConflicts), those of its possible attackers, and those the
% graph extensions relate it to. The conflicts come first, as the possible attackers are only looked for once.
dependency(_, Argument, S) :- attack::attackDependency(Argument, S).
dependency(Id, Argument, S) :- attack::possibleAttackers(Id, Argument, [S]).
dependency(_, Argument, S) :- graphExtension(X), X::queryDependencies(Argument, S).

% expand(+Locality): for the connected locality, builds the arguments whose labels may depend on the ones built
% (see dependant/2), with those they depend on in turn, until none is new
expand(connected) :-
    findall(Id, (context_check(clause(arg(Id), _)), context_absent(expanded(Id))), Ids),
    Ids \== [], !,
    findall(_, (
        utils::element(Id, Ids),
        context_assert(expanded(Id)),
        context_check(clause(arg(Id), argument(Argument))),
        dependant(Argument, Statement),
        findall(_, buildSubGraph(Statement, _), _)
    ), _),
    expand(connected).
expand(_).

% The statements whose arguments may depend on an argument: those it may attack, those its conclusion makes
% conflict (metaConflicts), those of its super-arguments, and those the graph extensions relate to it
dependant(Argument, S) :- attack::possiblyAttacked(Argument, S).
dependant(Argument, S) :- attack::attackDependant(Argument, S).
dependant([_, _, [C], _, _], S) :- parser::classic_rule(_, Premises, [S]), member(C, Premises).
dependant(Argument, S) :- graphExtension(X), X::queryDependants(Argument, S).


findSingleArgument(Query, Argument, Support) :-
    findArgument(Query, [], found(Groundings, [AllRules, TopRule, LastDefRules, DefRules, DefPremises], Support)),
    utils::deduplicate(DefRules, CDefRules),
    utils::deduplicate(DefPremises, CDefPremises),
    utils::deduplicate(Groundings, CGroundings),
    Argument = [AllRules, TopRule, Query, CGroundings, [LastDefRules, CDefRules, CDefPremises]].

% findArgument(Conclusion, Ids, found(Groundings, Rules, Support)) and
% findPremises(Premises, Ids, premises(Groundings, Rules, Support)): the results are packed in a single term,
% built in the clause bodies, so that clause heads only match the conclusion or the premises (matching many
% arguments against every clause head is what makes resolution expensive)

% Premise

findArgument(Conclusion, _, Found) :-
    parser::classic_rule(Id, Conclusion),
    premiseRules(Id, Rules),
    Found = found([], Rules, []).

% Rule

findArgument(Conclusion, Ids, Found) :-
    parser::classic_rule(Id, Premises, Conclusion),
    %\+ member(Id, Ids),
    findPremises(Premises, [Id|Ids], premises(Conclusions, ResRules, Support)),
    ruleRules(Id, ResRules, Rules),
    Found = found(Conclusions, Rules, Support).

findPremises([], _, Found) :-
    Found = premises([], [[], [], [], []], []).
% The premises are supported from the first, as the graph builder does: a Prolog check, or a contrary premise,
% may need the variables bound by the premises before it
findPremises([H|T], Ids, Found) :-
    parser::prolog([H], Term), !,
    (callable(Term) -> call(Term); Term),
    findPremises(T, Ids, Found).
findPremises([H|T], Ids, Found) :-
    parser::contrary([H], _), !,
    findPremises(T, Ids, premises(TConclusions, TRules, TSupport)),
    Found = premises([H|TConclusions], TRules, TSupport).
findPremises([H|T], Ids, Found) :-
    Sub = [AllRules, _, [PP], _, [LastDefRules, DefRules, DefPremises]],
    buildSubGraph(H, Sub),
    findPremises(T, Ids, premises(TConclusions, TRules, TSupport)),
    mergeRules([AllRules, [], LastDefRules, DefRules, DefPremises], TRules, Rules),
    Found = premises([PP|TConclusions], Rules, [Sub|TSupport]).

premiseRules(Id, Rules) :- parser::check_strict(Id), !, Rules = [[Id], none, [], [], []].
premiseRules(Id, [[Id], none, [], [], [Id]]).

% A single check_strict call: the second clause is only reached for defeasible rules
ruleRules(Id, [AllRules, LastDefRules, DefRules, DefPremises], Rules) :-
    parser::check_strict(Id), !,
    Rules = [[Id|AllRules], Id, LastDefRules, DefRules, DefPremises].
ruleRules(Id, [AllRules, _, DefRules, DefPremises], [[Id|AllRules], Id, [Id], [Id|DefRules], DefPremises]).

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
    context_absent(clause(conc(Conclusion), argument(Argument))),
    % write("Saving Argument: "),
    % write(Argument),nl,
    context_assert(newConc(T, Conclusion)),
    context_id(argument(Argument), Id),
    context_assert(argument(Argument)),
    context_assert(conc(Conclusion) :- argument(Argument)),
    context_assert(arg(Id) :- argument(Argument)),
    Argument = [_, _, _, _, Info],
    context_id(info(Info), InfoId),
    context_assert(argInfo(Id, InfoId, Info)).

saveSupports(_, []).
saveSupports(A, [B|T]) :-
    saveSupport(B, A),
    saveSupports(A, T).

saveSupport(A, B) :-
    context_absent(support(A, B)),
    % write("Saving Support: "),
    % write(A),write(" -> "),write(B),nl,
    context_assert(support(A, B)),
    context_id(argument(A), IdA),
    context_id(argument(B), IdB),
    context_assert(supportId(IdA, IdB)).


%==============================================================================
% OLD STUFF (USED IN ACTOR SOLVER)
%==============================================================================

%==============================================================================
% QUERY ALGORITHM
%==============================================================================

query(Query, Res) :-
    buildArgument(Query, Argument),
    once(defend(Argument, [], Res)).
query(Query, und) :- \+ buildArgument(Query, _).

% Check already evaluated arguments

defend(Argument, _, no) :- context_check(out(Argument)).
defend(Argument, _, und) :- context_check(und(Argument)).
defend(Argument, _, yes) :- context_check(in(Argument)).

% Exists a IN attacker -> OUT argument

defend(Argument, QueryChain, no) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == yes,
    bufferResult(Argument, no), !.

% Exists a UND attacker -> UND argument

defend(Argument, QueryChain, Res) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == und,
    ambiguityCheck(Argument, Attacker, Res),
    bufferResult(Argument, Res), !.

complementary([_, _, ConclusionA, _, _], [_, _, ConclusionB, _, _]) :-
    attack::expanded_conflict(ConclusionA, ConclusionB).

ambiguityCheck(A, B, no) :- ambiguityBlocking,
    \+ complementary(A, B),
    context_check(attack(direct, B, A, _)), !.
ambiguityCheck(_, _, und).

% Exists a cycle in the inference chain attacker -> UND argument

defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, no),
    bufferResult(Argument, und), !.

% IN in the other cases

defend(Argument, QueryChain, yes) :- bufferResult(Argument, yes).

bufferResult(Argument, no) :- context_absent(out(Argument)), context_assert(out(Argument)), !.
bufferResult(Argument, yes) :- context_absent(in(Argument)), context_assert(in(Argument)), !.
bufferResult(Argument, und) :- context_absent(und(Argument)), context_assert(und(Argument)), !.
bufferResult(_, _).


%==============================================================================
% ATTACKER RESEARCH
%==============================================================================

findAttacker(Target, QueryChain, Attacker, IsValid) :-
    attacker(Target, Attacker, Type),
    bufferAttacker(Attacker, Target, Type),
    detectCycle(Attacker, QueryChain, IsValid).

bufferAttacker(Attacker, Target, Type) :-
    context_absent(attack(Type, Attacker, Target, none)),
    context_assert(attack(Type, Attacker, Target, none)),
    context_id(argument(Attacker), IdAttacker),
    context_id(argument(Target), IdTarget),
    context_id(argument(none), IdNone),
    context_assert(attackId(Type, IdAttacker, IdTarget, IdNone)), !.
bufferAttacker(_, _, _).

detectCycle(Attacker, QueryChain, yes) :- \+ member(Attacker, QueryChain).
detectCycle(Attacker, QueryChain, no) :- member(Attacker, QueryChain).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type) :-
    member(X, Rules),
    \+ parser::strict(X),
    attackerOnRule(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type) :-
    member(X, Groundings),
    attackerOnTerm(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type).

% undercut

attackerOnRule(Rule, _, Argument, undercut) :-
    parser::undercut(Undercut, Rule),
    buildArgument(Undercut, Argument).

% rebut / undermine

attackerOnTerm(Term, [TargetRules, TopRule, Conclusion, Groundings, ArgInfo], Attacker, direct) :-
    \+ parser::contrary(Term, _),
    \+ strict([TargetRules, TopRule, Conclusion, Groundings, ArgInfo]),
    (graphExtension(rebutRestriction) ->
        (
            getSubArgument(Term, TargetRules, SubArgument),
            rebutRestriction(SubArgument)
        );
        true
    ),
    attack::expanded_conflict(Term, [X]),
    buildArgument(X, Attacker),
    (graphExtension(standardPref) ->
        (
            once(getSubArgument(Term, TargetRules, SubArgument)),
            \+ superiorArgument(SubArgument, Attacker)
        );
        true
    ).

% contrary-rebut / contrary-undermine

attackerOnTerm(Unless, _, Attacker, contrary) :-
    parser::contrary(Unless, X),
    buildArgument(X, Attacker).

% Strict arguments restriction

strict([_, _, _, _, [_, [], []]]).

% Rebut restriction

rebutRestriction([TargetRules, none, Conclusion, Groundings, ArgInfo]) :-
    graphExtension(rebutRestriction).
rebutRestriction([TargetRules, TopRule, Conclusion, Groundings, ArgInfo]) :-
    graphExtension(rebutRestriction),
    TopRule \= none,
    rebutRestriction::restrict([TargetRules, TopRule, Conclusion, Groundings, ArgInfo]).
rebutRestriction(_) :- \+ graphExtension(rebutRestriction).

% Preferences

superiorArgument(SubArgument, Attacker) :-
    superiority::superiorArgument(SubArgument, Attacker).

getSubArgument(Term, Rules, [SubRules, SubTopRule, SubConclusion, SubGrounds, Info]) :-
    context_check(clause(support(Term), argument([SubRules, SubTopRule, SubConclusion, SubGrounds, Info]))),
    contained(SubRules, Rules).

contained([], _).
contained([H|T], Target) :- member(H, Target), contained(T, Target).

%==============================================================================
% ARGUMENT CONSTRUCTION
%==============================================================================

buildArgument(Query, Argument) :-
    context_absent(explored(Query)),
    findall(_, (
        buildSingleArgument([Query], Argument),
        context_assert(argument(Argument))
    ), _),
    context_assert(explored(Query)),
    fail.
buildArgument(Query, [R, T, [Query], B, I]) :- context_check(argument([R, T, [Query], B, I])).

% buildArgument(Query, Argument) :- buildSingleArgument([Query], Argument).

buildSingleArgument(Query, Argument) :-
    build(Query, [], Groundings, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]),
    utils::deduplicate(DefRules, CDefRules),
    utils::deduplicate(DefPremises, CDefPremises),
    utils::deduplicate(Groundings, CGroundings),
    Argument = [AllRules, TopRule, Query, CGroundings, [LastDefRules, CDefRules, CDefPremises]].

% Body

build(Check, _, [], []) :- parser::prolog(Check, Term), (callable(Term) -> call(Term); Term), !.
build(Unless, _, [Unless], []) :- parser::contrary(Unless, _), !.

% Premise

build(Conclusion, _, [Conclusion], [AllRules, TopRule, LastDefRules, DefRules, DefPremises]) :-
    parser::premise(Id, Conclusion),
    premiseRules(Id, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]),
    context_assert(support(Conclusion) :- argument([AllRules, TopRule, Conclusion, [Conclusion], [LastDefRules, DefRules, DefPremises]])).

% Rule

build(Conclusion, Ids, [Conclusion|Conclusions], [AllRules, TopRule, LastDefRules, DefRules, DefPremises]) :-
    parser::rule(Id, Premises, Conclusion),
    \+ member(Id, Ids),
    buildPremises(Premises, [Id|Ids], Conclusions, ResRules),
    ruleRules(Id, ResRules, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]),
    context_assert(support(Conclusion) :- argument([AllRules, TopRule, Conclusion, [Conclusion|Conclusions], [LastDefRules, DefRules, DefPremises]])).

buildPremises([], _, [], [[], [], [], []]).
buildPremises([H|T], Ids, Conclusions, Rules) :-
    build([H], Ids, HConclusions, HRules),
    buildPremises(T, Ids, TConclusions, TRules),
    utils::appendLists([HConclusions, TConclusions], Conclusions),
    mergeRules(HRules, TRules, Rules).