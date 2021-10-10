computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    \+ queryMode,
    abstract:::computeGlobalAcceptance,
    mineResults(Goal, YesResult, NoResult, UndResult).

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    queryMode,
    argumentLabellingMode(grounded),
    parser::check_modifiers_in_list(effects, [Goal], [X]),
    findall(_, query(X, _), _),
    statementLabellingMode(Y),
    Y:::statementLabelling,
    mineResults(Goal, YesResult, NoResult, UndResult).

mineResults(Goal, YesResult, NoResult, UndResult) :-
    parser::check_modifiers_in_list(effects, [Goal], [X]),
    findall(Goal, context_check(statIn(X)), In),
    findall(Goal, context_check(statOut(X)), Out),
    findall(Goal, context_check(statUnd(X)), Und),
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

% Exists a cycle in the inference chain attacker -> UND argument

defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, no),
    bufferResult(Argument, und), !.

% Exists a UND attacker -> UND argument

defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == und,
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
    attacker(Target, Attacker),
    bufferAttacker(Attacker, Target),
    detectCycle(Attacker, QueryChain, IsValid).

bufferAttacker(Attacker, Target) :-
    \+ context_check(attack(none, Attacker, Target, none)),
    context_assert(attack(none, Attacker, Target, none)), !.
bufferAttacker(_, _).

detectCycle(Attacker, QueryChain, yes) :- \+ member(Attacker, QueryChain).
detectCycle(Attacker, QueryChain, no) :- member(Attacker, QueryChain).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument) :-
    member(X, Rules),
    \+ context_check(strict(X)),
    attackerOnRule(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument) :-
    member(X, Groundings),
    attackerOnTerm(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument).

% undercut

attackerOnRule(Rule, _, Argument) :-
    buildArgument([undercut(Rule)], Argument).

% rebut / undermine

attackerOnTerm(Term, [TargetRules, TopRule, Conclusion, Groundings, ArgInfo], Attacker) :-
    Term \= [unless, _],
    \+ strict([TargetRules, TopRule, Conclusion, Groundings, ArgInfo]),
    buildSubArgument(Term, TargetRules, SubArgument),
    rebutRestriction(SubArgument),
    standard_af::conflict(Term, X),
    buildArgument(X, Attacker),
    \+ superiorArgument(SubArgument, Attacker).

% contrary-rebut / contrary-undermine

attackerOnTerm([unless, X], _, Attacker) :-
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

buildSubArgument(Term, Rules, [SubRules, SubTopRule, SubConclusion, SubGrounds, Info]) :-
    buildArgument(Term, [SubRules, SubTopRule, SubConclusion, SubGrounds, Info]),
    contained(SubRules, Rules).

contained([], _).
contained([H|T], Target) :- member(H, Target), contained(T, Target).

%==============================================================================
% ARGUMENT CONSTRUCTION
%==============================================================================

buildArgument(Query, Argument) :-
    \+ context_check(explored(Query)),
    findall(_, (
        buildSingleArgument(Query, Argument),
        context_assert(argument(Argument))
    ), _),
    context_assert(explored(Query)),
    fail.
buildArgument(Query, [R, T, Query, B, I]) :- context_check(argument([R, T, Query, B, I])).

buildSingleArgument(Query, Argument) :-
    build(Query, Groundings, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]),
    utils::deduplicate(DefRules, CDefRules),
    utils::deduplicate(DefPremises, CDefPremises),
    utils::deduplicate(Groundings, CGroundings),
    Argument = [AllRules, TopRule, Query, CGroundings, [LastDefRules, CDefRules, CDefPremises]].

% Premise

build(Conclusion, [Conclusion], Rules) :-
    context_check(premise([Id, Conclusion])),
    premiseRules(Id, Rules).

% Rule

build(Conclusion, [Conclusion|Conclusions], Rules) :-
    context_check(rule([Id, Premises, Conclusion])),
    buildPremises(Premises, Conclusions, ResRules),
    ruleRules(Id, ResRules, Rules).

build([prolog(Check)], [], []) :- (callable(Check) -> call(Check); Check).
build([unless, Atom], [[unless, Atom]], []).

buildPremises([], [], [[], [], [], []]).
buildPremises([H|T], Conclusions, Rules) :-
    build(H, HConclusions, HRules),
    buildPremises(T, TConclusions, TRules),
    utils::appendLists([HConclusions, TConclusions], Conclusions),
    mergeRules(HRules, TRules, Rules).

premiseRules(Id, [[Id], none, [], [], [Id]]) :- \+ context_check(strict(Id)).
premiseRules(Id, [[Id], none, [], [], []]) :- context_check(strict(Id)).

ruleRules(Id, [AllRules, _, DefRules, DefPremises], 
    [[Id|AllRules], Id, [Id], [Id|DefRules], DefPremises]) :- \+ context_check(strict(Id)).
ruleRules(Id, [AllRules, LastDefRules, DefRules, DefPremises], 
    [[Id|AllRules], Id, LastDefRules, DefRules, DefPremises]) :- context_check(strict(Id)).

mergeRules([], [AllRules, LastDefRules, DefRules, DefPremises], [AllRules, LastDefRules, DefRules, DefPremises]).
mergeRules([HAR, _, HLDR, HDR, HDP], [TAR, TLDR, TDR, TDP], [AR, LDR, DR, DP]) :-
   utils::appendLists([HAR, TAR], AR),
   utils::appendLists([HLDR, TLDR], LDR),
   utils::appendLists([HDR, TDR], DR),
   utils::appendLists([HDP, TDP], DP).
