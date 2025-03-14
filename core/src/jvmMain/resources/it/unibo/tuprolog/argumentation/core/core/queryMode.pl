computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    \+ queryMode,
    abstract:::computeGlobalAcceptance,
    mineResults(Goal, YesResult, NoResult, UndResult).

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    queryMode,
    argumentLabellingMode(grounded),
    findall(_, query(Goal, _), _),
    (ambiguityBlocking ->
        statement_binary:::statementLabelling;
        statementLabellingMode(Y),
        Y:::statementLabelling
    ),
    mineResults(Goal, YesResult, NoResult, UndResult).

mineResults(Goal, YesResult, NoResult, UndResult) :-
    findall(Goal, context_check(statIn([Goal])), In),
    findall(Goal, context_check(statOut([Goal])), Out),
    findall(Goal, context_check(statUnd([Goal])), Und),
    utils::sort(In, YesResult),
    utils::sort(Out, NoResult),
    utils::sort(Und, UndResult).


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

bufferResult(Argument, no) :- \+ context_check(out(Argument)), context_assert(out(Argument)), !.
bufferResult(Argument, yes) :- \+ context_check(in(Argument)), context_assert(in(Argument)), !.
bufferResult(Argument, und) :- \+ context_check(und(Argument)), context_assert(und(Argument)), !.
bufferResult(_, _).


%==============================================================================
% ATTACKER RESEARCH
%==============================================================================

findAttacker(Target, QueryChain, Attacker, IsValid) :-
    attacker(Target, Attacker, Type),
    bufferAttacker(Attacker, Target, Type),
    detectCycle(Attacker, QueryChain, IsValid).

bufferAttacker(Attacker, Target, Type) :-
    \+ context_check(attack(Type, Attacker, Target, none)),
    context_assert(attack(Type, Attacker, Target, none)), !.
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
    \+ context_check(explored(Query)),
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
