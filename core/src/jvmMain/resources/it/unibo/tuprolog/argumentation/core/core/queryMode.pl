computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    queryMode,
    argumentLabellingMode(grounded),
    check_modifiers_in_list(effects, [Goal], [X]),
    findall([X, Res], query(X, Res), Result),
    populateResultSets(Result, ArgsIn, ArgsOut, ArgsUnd),
    beautifyResult(Goal, ArgsIn, ArgsOut, ArgsUnd, YesResult, NoResult, UndResult), !.

computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    \+ queryMode,
    computeGlobalAcceptance([_, _, _], [In, Out, Und]),
    findall(X, member([_, _, X], In), ArgsIn),
    findall(X, member([_, _, X], Out), ArgsOut),
    findall(X, member([_, _, X], Und), ArgsUnd),
    beautifyResult(Goal, ArgsIn, ArgsOut, ArgsUnd, YesResult, NoResult, UndResult), !.

beautifyResult(Goal, ArgsIn, ArgsOut, ArgsUnd, In, Out, Und) :-
    findall(Goal, answerSingleQuery(Goal, ArgsIn), In),
    findall(Goal, answerSingleQuery(Goal, ArgsOut), Out),
    findall(Goal, answerSingleQuery(Goal, ArgsUnd), Und).

answerSingleQuery(Goal, Args) :-
    check_modifiers_in_list(effects, [Goal], [X]),
    findall(Y, member(Y, Args), Set),
    member(X, Set).

populateResultSets([], [], [], []).
populateResultSets([[Query,yes]|T], [Query|Yes], No, Und) :- populateResultSets(T, Yes, No, Und).
populateResultSets([[Query,no]|T], Yes, [Query|No], Und) :- populateResultSets(T, Yes, No, Und).
populateResultSets([[Query,und]|T], Yes, No, [Query|Und]) :- populateResultSets(T, Yes, No, Und).

query(Query, Res) :-
    buildArgument(Query, Argument),
    write(Argument),nl,
    once(defend(Argument, [], Res)).
query(Query, und) :- \+ buildArgument(Query, _).

% Poco ottimizzato: mentre cerco un In potrei già trovare un Und (va bufferizzato)
% Caso base: esiste fra i miei attaccanti un verificato -> io sono out
defend(Argument, QueryChain, no) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == yes, !.
% Caso ciclo: esiste fra i miei attaccanti un argomento già incontrato (ciclo) sono Und
defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, no), !.
% Caso indeterminatezza: con solo un attaccante Und sono Und
defend(Argument, QueryChain, und) :-
    findAttacker(Argument, QueryChain, Attacker, yes),
    once(defend(Attacker, [Argument|QueryChain], X)),
    X == und, !.
% Negli altri casi sono In
defend(Argument, QueryChain, yes).

% Considero le mie parti attaccabili
%  - Rules per undercut e rebut
%  - Premises per undercut e undermine
%  - Attenzione al corpo delle regole, per contrary-undercut e contrary-undermine
findAttacker(Target, QueryChain, Attacker, IsValid) :-
    attacker(Target, Attacker),
    findAttackerCicle(Attacker, QueryChain, IsValid).

findAttackerCicle(Attacker, QueryChain, yes) :- \+ member(Attacker, QueryChain).
findAttackerCicle(Attacker, QueryChain, no) :- member(Attacker, QueryChain).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument) :-
    member(X, Rules),
    \+ strict(X),
    attackerOnRule(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument).

attacker([Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument) :-
    member(X, Groundings),
    attackerOnTerm(X, [Rules, TopRule, Conclusion, Groundings, ArgInfo], Argument).

% undercut
attackerOnRule(Rule, _, Argument) :-
    buildArgument([undercut(Rule)], Argument).

% rebut e undermine
attackerOnTerm(Term, [TargetRules, TargetTopRule, TargetConc, _, [LDRA, DRA, DPA]], [Rules, TopRule, Conc, Grondings, [LDRB, DRB, DPB]]) :-
    Term \== [unless, _],
    conflict(Term, X),
    buildArgument(X, [Rules, TopRule, Conc, Grondings, [LDRB, DRB, DPB]]),
    restrict([Rules, TopRule, Conc], [TargetRules, TargetTopRule, TargetConc]),
    \+ superiorArgumentStructured(LDRA, DRA, DPA, LDRB, DRB, DPB, Term, TargetRules).

superiorArgumentStructured(LDRA, DRA, DPA, LDRB, DRB, DPB, TargetTerm, TargetRules) :-
    orderingComparator(normal),
    buildArgument(TargetTerm, [Rules, _, _, _, [LDRC, DRC, DPC]]),
    contained(Rules, TargetRules),
    superiorArgument(LDRC, DRC, DPC, LDRB, DRB, DPB).
superiorArgumentStructured(LDRA, DRA, DPA, LDRB, DRB, DPB, _, _) :-
    \+ orderingComparator(normal),
    superiorArgument(LDRA, DRA, DPA, LDRB, DRB, DPB).

contained([], _).
contained([H|T], Target) :- member(H, Target), contained(T, Target).

% contrary-rebut and contrary-undermine
attackerOnTerm([unless, X], [Rules, TopRule, Conclusion, _, _], [XR, XTR, XC, XG, XI]) :-
    buildArgument(X, [XR, XTR, XC, XG, XI]),
    restrict([XR, XTR, XC], [Rules, TopRule, Conclusion]).

% Ricorsione sul corpo di 
%   - rule([id, [premises], conclusion])
%   - premise([id, conclusion])
% fino ad arrivare a una regola senza premesse o ad una premessa
% Mi porto dietro il grounding dei termini per poter derivare gli attacchi e le info sulla derivazione dell'argomento (defRules, lastDefRules, defPremises)
buildArgument(Query, Argument) :-
    build(Query, Groundings, [AllRules, TopRule, LastDefRules, DefRules, DefPremises]),
    once(deduplicate(DefRules, CDefRules)),
    once(deduplicate(DefPremises, CDefPremises)),
    once(deduplicate(Groundings, CGroundings)),
    Argument = [AllRules, TopRule, Query, CGroundings, [LastDefRules, CDefRules, CDefPremises]].

premiseRules(Id, [[Id], none, [], [], [Id]]) :- \+ strict(Id).
premiseRules(Id, [[Id], none, [], [], []]) :- strict(Id).

ruleRules(Id, [AllRules, _, DefRules, DefPremises], 
    [[Id|AllRules], Id, [Id], [Id|DefRules], DefPremises]) :- \+ strict(Id).
ruleRules(Id, [AllRules, LastDefRules, DefRules, DefPremises], 
    [[Id|AllRules], Id, LastDefRules, DefRules, DefPremises]) :- strict(Id).

% Da qui posso prendere la top Rule e le def premises
build(Conclusion, [Conclusion], Rules) :-
    premise([Id, Conclusion]),
    premiseRules(Id, Rules).

% Da qui posso prendere la top rule, le DefRules e le LastDefRules 
build(Conclusion, [Conclusion|Concls], Rules) :-
    rule([Id, Premises, Conclusion]),
    buildPremises(Premises, Concls, ResRules),
    ruleRules(Id, ResRules, Rules).

build([prolog(Check)], [], []) :- (callable(Check) -> call(Check); Check).
build([unless, Atom], [[unless, Atom]], []).

mergeRules([], [AllRules, LastDefRules, DefRules, DefPremises], [AllRules, LastDefRules, DefRules, DefPremises]).
mergeRules([HAR, _, HLDR, HDR, HDP], [TAR, TLDR, TDR, TDP], [AR, LDR, DR, DP]) :-
   appendLists([HAR, TAR], AR),
   appendLists([HLDR, TLDR], LDR),
   appendLists([HDR, TDR], DR),
   appendLists([HDP, TDP], DP).

buildPremises([], [], [[], [], [], []]).
buildPremises([H|T], Concls, Rules) :-
    build(H, HConcls, HRules),
    buildPremises(T, TConcls, TRules),
    appendLists([HConcls, TConcls], Concls),
    mergeRules(HRules, TRules, Rules).

deduplicate([], []).
deduplicate(List, Output) :- List \== [], setof(X, member(X, List), Output).

% isSkepticallyAcceptable(Goal) :-
%     convertAllRules,
%     findall(STATIN, computeGlobalAcceptance([STATIN, _, _], [_, _, _]), SOLUTIONS),
%     check_modifiers_in_list(effects, [Goal], [X]),
%     all(X, SOLUTIONS), !.

% all(_, []).
% all(Goal, [H|T]) :- member(Goal, H), all(Goal, T).

% isCredulouslyAcceptable(Goal) :-
%     convertAllRules,
%     computeGlobalAcceptance([STATIN, _, _], [_, _, _]),
%     answerSingleQuery(Goal, STATIN), !.
