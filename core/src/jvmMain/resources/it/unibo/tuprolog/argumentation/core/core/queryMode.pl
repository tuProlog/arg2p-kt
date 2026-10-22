computeStatementAcceptance(Goal, Res) :-
    computeStatementAcceptance(Goal),
    mineResults(Goal, Res).

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    computeStatementAcceptance(Goal),
    mineResults(Goal, YesResult, NoResult, UndResult).

computeStatementAcceptance(Goal) :-
    \+ setting(queryMode),
    abstract:::computeGlobalAcceptance.

computeStatementAcceptance(Goal) :-
    setting(queryMode),
    locality(Locality),
    queryAcceptance(Locality, Goal).

% The part of the graph the labels of the arguments for the query depend on, as the semantics declares it
% (locality/1): the arguments they depend on (directional), those connected to them in either direction
% (connected), or the whole graph (whole_graph, also for a semantics that does not declare it)
locality(Locality) :-
    setting(argumentLabellingMode, Semantics),
    Semantics::locality(L), !,
    Locality = L.
locality(whole_graph).

queryAcceptance(whole_graph, _) :- !,
    abstract:::computeGlobalAcceptance.
queryAcceptance(Locality, Goal) :-
    explore(Goal),
    expand(Locality),
    setting(graphBuildMode, X),
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

% explore(+Query): builds the arguments for Query and those their labels may depend on (see buildSubGraph/2).
% With cycles among the rules, a statement met again while its arguments are being built only gets those built so
% far: the exploration is then repeated, from scratch, until it builds no new argument (the arguments are finitely
% many, as none uses a rule twice).
explore(Query) :-
    context_retract(newConc(query, _)),
    context_retract(cycle),
    findall(_, buildSubGraph(Query, _), _),
    (context_check(cycle), context_check(newConc(query, _)) ->
        context_retract(explored(_)),
        explore(Query)
    ;
        true
    ).

% buildSubGraph(+Query, ?Argument): builds the arguments for Query, and those their labels may depend on
% (see dependency/3), recursively; then Argument is each argument for Query. A statement is explored once (or
% once more for each round of explore/1): explored(Query) covers the statements Query generalises.
buildSubGraph(Query, _) :-
    ground_vars(Query, Normalized),
    context_absent(explored(Normalized)),
    context_assert(explored(Query)),
    context_assert(exploring(Normalized, Query)),
    findall(_, (
        findSingleArgument([Query], Argument, Support),
        recordArgument(Query, Argument, Support),
        context_id(argument(Argument), Id),
        dependency(Id, Argument, Statement),
        buildSubGraph(Statement, _)
    ), _),
    context_retract(exploring(Normalized, _)),
    fail.
% A statement being explored (by a goal that generalises it): a cycle, as its arguments are not all built yet
buildSubGraph(Query, _) :-
    ground_vars(Query, Normalized),
    context_check(exploring(_, Normalized)),
    context_absent(cycle),
    context_assert(cycle),
    fail.
buildSubGraph(Query, Argument) :- context_check(clause(conc([Query]), argument(Argument))).

% The statements whose arguments the label of an argument may depend on, besides its sub-arguments (built with
% it): those that make others conflict with it (metaConflicts), those of its possible attackers, and those the
% graph extensions relate it to. The conflicts come first, as the possible attackers are only looked for once.
dependency(_, Argument, S) :- attack::attackDependency(Argument, S).
dependency(Id, Argument, S) :- attack::possibleAttackers(Id, Argument, [S]).
dependency(_, Argument, S) :- setting(graphExtension, X), X::queryDependencies(Argument, S).

% expand(+Locality): for the connected locality, builds the arguments whose labels may depend on the ones built
% (see dependant/2), with those they depend on in turn, until none is new
expand(connected) :-
    findall(Id, (context_check(clause(arg(Id), _)), context_absent(expanded(Id))), Ids),
    Ids \== [], !,
    findall(_, (
        member(Id, Ids),
        context_assert(expanded(Id)),
        context_check(clause(arg(Id), argument(Argument))),
        dependant(Argument, Statement),
        explore(Statement)
    ), _),
    expand(connected).
expand(_).

% The statements whose arguments may depend on an argument: those it may attack, those its conclusion makes
% conflict (metaConflicts), those of its super-arguments, and those the graph extensions relate to it
dependant(Argument, S) :- attack::possiblyAttacked(Argument, S).
dependant(Argument, S) :- attack::attackDependant(Argument, S).
dependant([_, _, [C], _, _], S) :- parser::classic_rule(_, Premises, [S]), member(C, Premises).
dependant(Argument, S) :- setting(graphExtension, X), X::queryDependants(Argument, S).


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

% An argument uses a rule at most once (as in the whole graph, see utils::argument_rules)
findArgument(Conclusion, Ids, Found) :-
    parser::classic_rule(Id, Premises, Conclusion),
    findPremises(Premises, [Id|Ids], premises(Conclusions, ResRules, Support)),
    ResRules = [SubRules|_],
    \+ member(Id, SubRules),
    ruleRules(Id, ResRules, Rules),
    Found = found(Conclusions, Rules, Support).

% The premises of a rule are supported one at a time, most selective first (see nextPremise/2): with the
% bindings of the conclusion and of the premises supported so far, rather than in the order they are written,
% which could build all the arguments for a premise that the following ones then discard. Each premise has a slot
% for its result, and the results are collected in the order of the rule: an argument is the same however the
% premises were supported.
findPremises(Premises, _, Found) :-
    premiseSlots(Premises, Slots),
    supportPremises(Slots),
    collectPremises(Slots, Found).

premiseSlots([], []).
premiseSlots([Premise|Premises], [slot(Premise, _)|Slots]) :-
    premiseSlots(Premises, Slots).

supportPremises(Slots) :-
    nextPremise(Slots, Slot), !,
    supportPremise(Slot),
    supportPremises(Slots).
supportPremises(_).

% The next premise to support: a Prolog check as soon as the ordinary premises written before it are supported
% (with the bindings the graph builder gives it, as it may bind variables for the premises after it) or it is
% ground (when it can only discard); otherwise the premise concluded by the fewest rules; the contrary premises,
% which are only recorded, last
nextPremise(Slots, Slot) :-
    readyCheck(Slots, true, Slot), !.
nextPremise(Slots, Slot) :-
    mostSelective(Slots, none, best(Slot, _)), !.
nextPremise(Slots, Slot) :-
    member(Slot, Slots),
    Slot = slot(_, Result),
    var(Result), !.

% readyCheck(+Slots, +Supported, -Slot): the first pending Prolog check that can run, Supported telling whether
% the ordinary premises before the slots are all supported
readyCheck([Slot|_], Supported, Slot) :-
    Slot = slot(Premise, Result),
    var(Result),
    parser::prolog([Premise], _),
    (Supported == true ; ground(Premise)), !.
readyCheck([slot(Premise, Result)|Slots], Supported, Slot) :-
    (var(Result), ordinaryPremise(Premise) -> Next = false ; Next = Supported),
    readyCheck(Slots, Next, Slot).

% The pending ordinary premise concluded by the fewest rules: each count stops at the best one so far
mostSelective([], Best, Best).
mostSelective([Slot|Slots], Best, Selected) :-
    Slot = slot(Premise, Result),
    var(Result),
    ordinaryPremise(Premise), !,
    countLimit(Best, Limit),
    parser::candidates(Premise, Limit, Count),
    selective(Slot, Count, Best, Next),
    mostSelective(Slots, Next, Selected).
mostSelective([_|Slots], Best, Selected) :-
    mostSelective(Slots, Best, Selected).

countLimit(none, none).
countLimit(best(_, Count), Count).

selective(Slot, Count, none, best(Slot, Count)) :- !.
selective(Slot, Count, best(_, Min), best(Slot, Count)) :- Count < Min, !.
selective(_, _, Best, Best).

ordinaryPremise(Premise) :-
    \+ parser::prolog([Premise], _),
    \+ parser::contrary([Premise], _).

supportPremise(slot(Premise, Result)) :-
    parser::prolog([Premise], Term), !,
    (callable(Term) -> call(Term); Term),
    Result = check.
supportPremise(slot(Premise, Result)) :-
    parser::contrary([Premise], _), !,
    Result = contrary.
supportPremise(slot(Premise, Result)) :-
    Sub = [_, _, [_], _, _],
    buildSubGraph(Premise, Sub),
    Result = argument(Sub).

collectPremises([], Found) :-
    Found = premises([], [[], [], [], []], []).
collectPremises([slot(_, check)|Slots], Found) :- !,
    collectPremises(Slots, Found).
collectPremises([slot(Premise, contrary)|Slots], Found) :- !,
    collectPremises(Slots, premises(Conclusions, Rules, Support)),
    Found = premises([Premise|Conclusions], Rules, Support).
collectPremises([slot(_, argument(Sub))|Slots], Found) :-
    collectPremises(Slots, premises(TConclusions, TRules, TSupport)),
    Sub = [AllRules, _, [PP], _, [LastDefRules, DefRules, DefPremises]],
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

% recordArgument(+Query, +Argument, +Support): Argument saved with its supports, unless it already was (built in a
% previous round of explore/1, whose dependencies are explored again)
recordArgument(Query, Argument, Support) :-
    saveArgument(query, [Query], Argument), !,
    saveSupports(Argument, Support).
recordArgument(_, _, _).

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

ambiguityCheck(A, B, no) :- setting(ambiguityBlocking),
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
    (setting(graphExtension, rebutRestriction) ->
        (
            getSubArgument(Term, TargetRules, SubArgument),
            rebutRestriction(SubArgument)
        );
        true
    ),
    attack::expanded_conflict(Term, [X]),
    buildArgument(X, Attacker),
    (setting(graphExtension, standardPref) ->
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
    setting(graphExtension, rebutRestriction).
rebutRestriction([TargetRules, TopRule, Conclusion, Groundings, ArgInfo]) :-
    setting(graphExtension, rebutRestriction),
    TopRule \= none,
    rebutRestriction::restrict([TargetRules, TopRule, Conclusion, Groundings, ArgInfo]).
rebutRestriction(_) :- \+ setting(graphExtension, rebutRestriction).

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