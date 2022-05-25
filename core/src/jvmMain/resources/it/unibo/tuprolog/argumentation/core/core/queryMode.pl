computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    \+ queryMode,
    abstract:::computeGlobalAcceptance,
    mineResults(Goal, YesResult, NoResult, UndResult).

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
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
    buildArgument(Query, [], Argument),
    write(Argument),nl,
    once(defend(Argument, [], Res)),
    write(Res),nl.
query(Query, und) :- write(und),nl.

% Check already evaluated arguments

defend(Argument, _, no) :- context_check(out(Argument)).
defend(Argument, _, und) :- context_check(und(Argument)).
defend(Argument, _, yes) :- context_check(in(Argument)).

% Exists a IN attacker -> OUT argument

defend(Argument, QueryChain, no) :-
    findAttacker(Argument, Attacker),
    detectCycle(Attacker, QueryChain, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    context_assert(att(Attacker, Argument, X)),
    X == yes,
    bufferResult(Argument, no), !.

% Exists a UND attacker -> UND argument

defend(Argument, QueryChain, Res) :-
    context_check(att(Attacker, Argument, und)),
    ambiguityCheck(Argument, Attacker, Res),
    bufferResult(Argument, Res), !.

% Exists a cycle in the inference chain attacker -> UND argument

defend(Argument, QueryChain, und) :-
    context_check(attack(_, Attacker, Argument, _)),
    detectCycle(Attacker, QueryChain, no),
    bufferResult(Argument, und), !.

% IN in the other cases

defend(Argument, QueryChain, yes) :- bufferResult(Argument, yes).

% Ambiguity check

complementary([_, _, ConclusionA, _, _], [_, _, ConclusionB, _, _]) :-
    standard_af::conflict(ConclusionA, ConclusionB).

ambiguityCheck(A, B, no) :- ambiguityBlocking,
    \+ complementary(A, B),
    context_check(attack(direct, B, A, _)), !.
ambiguityCheck(_, _, und).

bufferResult(Argument, no) :- \+ context_check(out(Argument)), context_assert(out(Argument)), !.
bufferResult(Argument, yes) :- \+ context_check(in(Argument)), context_assert(in(Argument)), !.
bufferResult(Argument, und) :- \+ context_check(und(Argument)), context_assert(und(Argument)), !.
bufferResult(_, _).

% Cycle detection

detectCycle(Attacker, QueryChain, no) :- member(Attacker, QueryChain).
detectCycle(Attacker, QueryChain, yes) :- \+ member(Attacker, QueryChain).


%==============================================================================
% ATTACKER RESEARCH
%==============================================================================

findAttacker(Target, Attacker) :-
    attacker(Target, Attacker, Type),
    bufferAttacker(Attacker, Target, Type).

bufferAttacker(Attacker, Target, Type) :-
    \+ context_check(attack(Type, Attacker, Target, none)),
    context_assert(attack(Type, Attacker, Target, none)), !.
bufferAttacker(_, _, _).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type) :-
    member(X, Rules),
    \+ check_strict(X),
    attackerOnRule(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type) :-
    member(X, Groundings),
    attackerOnTerm(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument, Type).

% undercut

attackerOnRule(Rule, _, Argument, undercut) :-
    undercut(Undercut, Rule),
    once(context_check(clause(rl(Undercut), _))),
    buildArgument(Undercut, [], Argument).

% rebut / undermine

attackerOnTerm(Term, [TargetRules, TopRule, Conclusion, Groundings, ArgInfo], Attacker, direct) :-
    \+ contrary(Term, _),
    \+ strict([TargetRules, TopRule, Conclusion, Groundings, ArgInfo]),
    (graphExtension(rebutRestriction) ->
        (
            getSubArgument(Term, TargetRules, SubArgument),
            rebutRestriction(SubArgument)
        );
        true
    ),
    standard_af::conflict(Term, [X]),
    once(context_check(clause(rl(X), _))),
    buildArgument(X, [], Attacker),
    (graphExtension(standardPref) ->
        (
            once(getSubArgument(Term, TargetRules, SubArgument)),
            \+ superiorArgument(SubArgument, Attacker)
        );
        true
    ).

% contrary-rebut / contrary-undermine

attackerOnTerm(Unless, _, Attacker, contrary) :-
    contrary(Unless, X),
    once(context_check(clause(rl(X), _))),
    buildArgument(X, [], Attacker).

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
    context_check(clause(conc(Term), argument([SubRules, SubTopRule, SubConclusion, SubGrounds, Info]))),
    contained(SubRules, Rules).

contained([], _).
contained([H|T], Target) :- member(H, Target), contained(T, Target).

%==============================================================================
% ARGUMENT CONSTRUCTION
%==============================================================================

buildArgument(Query, Ids, Argument) :-
%    \+ (context_check(explored(Y)), utils::check_structure(Query, Y)),
    \+ context_check(explored(Query)),
    findall(_, (
        buildSingleArgument([Query], Ids, Argument),
        context_assert(explored(Query)),
        \+ context_check(argument(Argument)),
        context_assert(argument(Argument)),
        context_assert(conc([Query]) :- argument(Argument))
%        write(Argument),nl
    ), _),
    fail.
buildArgument(Query, _, Argument) :- context_check(clause(conc([Query]), argument(Argument))).

% buildArgument(Query, Ids, Argument) :- buildSingleArgument([Query], Ids, Argument), context_assert(conc([Query]) :- argument(Argument)), write(Argument), nl.

buildSingleArgument(Query, Ids, Argument) :-
     build(Query, Ids, Groundings, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]),
     utils::deduplicate(DefRules, CDefRules),
     utils::deduplicate(DefPremises, CDefPremises),
     utils::deduplicate(Groundings, CGroundings),
     Argument = [AllRules, TopRule, Query, CGroundings, [LastDefRules, CDefRules, CDefPremises]].

build(Check, _, [], [[], [], [], [], []]) :- prolog(Check, Term), (callable(Term) -> call(Term); Term), !.
build(Unless, _, [Unless], [[], [], [], [], []]) :- contrary(Unless, _), !.

build(Conclusion, Ids, [Conclusion|Conclusions], [AllRules, TopRule, LastDefRules, DefRules, DefPremises]) :-
    rule(Id, Premises, Conclusion),
    \+ member(Id, Ids),
    buildPremises(Premises, [Id|Ids], Conclusions, ResRules),
    ruleRules(Id, ResRules, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]).

build(Conclusion, _, [Conclusion], [AllRules, TopRule, LastDefRules, DefRules, DefPremises]) :-
    premise(Id, Conclusion),
    premiseRules(Id, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]).

buildPremises((H, T), Ids, Conclusions, Rules) :-
    buildArgument(H, Ids, [AllRules, _, _, Groundings, [LastDefRules, DefRules, DefPremises]]),
    buildPremises(T, Ids, TConclusions, TRules),
    utils::append_fast(Groundings, TConclusions, Conclusions),
    mergeRules([AllRules, LastDefRules, DefRules, DefPremises], TRules, Rules).
buildPremises(H, Ids, Groundings, [AllRules, LastDefRules, DefRules, DefPremises]) :-
    H \= (_ , _),
    buildArgument(H, Ids, [AllRules, _, _, Groundings, [LastDefRules, DefRules, DefPremises]]).

premiseRules(Id, [[Id], none, [], [], []]) :- parser::check_strict(Id), !.
premiseRules(Id, [[Id], none, [], [], [Id]]).

ruleRules(Id, [AllRules, LastDefRules, DefRules, DefPremises],
    [[Id|AllRules], Id, LastDefRules, DefRules, DefPremises]) :- parser::check_strict(Id), !.
ruleRules(Id, [AllRules, _, DefRules, DefPremises],
    [[Id|AllRules], Id, [Id], [Id|DefRules], DefPremises]).

mergeRules([HAR, HLDR, HDR, HDP], [TAR, TLDR, TDR, TDP], [AR, LDR, DR, DP]) :-
   utils::append_fast(HAR, TAR, AR),
   utils::append_fast(HLDR, TLDR, LDR),
   utils::append_fast(HDR, TDR, DR),
   utils::append_fast(HDP, TDP, DP).

% THEORY

rule(Id, Premises, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Premises, Conclusion])).
premise(Id, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Conclusion])).
check_strict(Id) :- context_check(strict(Id)).

prolog([prolog(Term)], Term).
contrary([~(Term)], Term).
undercut(undercut(Rule), Rule).
superiority(sup(Id1, Id2), Id1, Id2).


%-----------------------------------------------------------------------------------------------------------------------
% BUILD TEMPLATE
%-----------------------------------------------------------------------------------------------------------------------

build_t(Check, _, [], []) :- prolog(Check, _), !.
build_t(Unless, _, [], []) :- contrary(Unless, _), !.

build_t(Conclusion, Ids, [], [AllRules, TopRule, LastDefRules, DefRules, DefPremises]) :-
    rule(Id, Premises, Conclusion),
    \+ member(Id, Ids),
    buildPremises_t(Premises, [Id|Ids], _, ResRules),
    ruleRules(Id, ResRules, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]).

build_t(Conclusion, _, [], []) :- once(premise(Id, C)).

buildPremises_t((H, T), Ids, [], Rules) :-
    build_t([H], Ids, _, HRules),
    buildPremises_t(T, Ids, _, TRules),
    mergeRules(HRules, TRules, Rules).
buildPremises_t(H, Ids, [], Rules) :-
    H \= (_ , _),
    build_t([H], Ids, _, HRules),
    mergeRules(HRules, [[], [], [], []], Rules).

%-----------------------------------------------------------------------------------------------------------------------
% Recover Body
%-----------------------------------------------------------------------------------------------------------------------

instantiate_chain([Rules, Top, Last, DefPrem], Template) :-
    instantiate_chain_support(Rules, Template).

instantiate_chain_support([], []).
instantiate_chain_support([Id|Rules], (Premises, Supports)) :-
    rule(Id, Premises, [X]),
    instantiate_chain_support(Rules, Supports).