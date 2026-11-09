%! holds(+Check)
%  Check, a Prolog premise of a rule, holds in the theory of the user.
holds(Check) :-
    (callable(Check) -> user::call(Check) ; user::Check).

%! strictness(+Id, -Strictness)
%  The rule Id is strict or defeasible (see checkStrict/1).
strictness(Id, strict) :- checkStrict(Id), !.
strictness(_, defeasible).

% The rule Id as written (and as the languages give it, see rulesRead/0), concluding Conclusion from Premises
ruleWritten(Id, PP, [Conclusion]) :-
    rulesRead,
    ruleClause(Id, Premises, Conclusion, Head, Body),
    cache_check(clause(Head, Body)),
    premiseList(Premises, PP).
premise(Id, [Conclusion]) :-
    rulesRead,
    premiseClause(Id, Conclusion, Head, Body),
    cache_check(clause(Head, Body)).

%! writtenRules(-Rules)
%  The rules and premises as they are written: [Id, Premises, Conclusion] or [Id, Conclusion] (see ruleParts/4).
writtenRules(Rules) :-
    rulesRead,
    clauseHead(_, Head),
    findall(Body, cache_check(clause(Head, Body)), Rules).

%! ruleClauses(-Clauses)
%  The clauses of the rules and premises, as the module rule writes them.
ruleClauses(Clauses) :-
    rulesRead,
    clauseHead(_, Head),
    findall((Head :- Body), cache_check(clause(Head, Body)), Clauses).

%! countConcluding(+Statement, +Limit, -Count)
%  The number of rules and premises concluding Statement, counted up to
%  Limit (an integer, or anything else for no limit)
countConcluding(Statement, Limit, Count) :-
    rulesRead,
    clauseHead(Statement, Head),
    cache_count(clause(Head, _), Limit, Count).

%! checkStrict(?Id)
%  The rule or premise Id is strict.
checkStrict(Id) :- rulesRead, cache_check(strict(Id)).

%! tupleToList(+Conjunction, -List)
%  The items of a conjunction, as a list (built with an accumulator rather than appending the lists of the two
%  sides, which took a module call at every step)
tupleToList(T, L) :- tupleToList(T, L, []).
tupleToList(A, [A|R], R) :- var(A), !.
tupleToList((A, B), L, R) :- !, tupleToList(A, L, M), tupleToList(B, M, R).
tupleToList(A, [A|R], R).

%! premiseList(+Premises, -List)
%  The premises of a rule, as written, as a list.
premiseList(Premises, List) :-
    tupleToList(Premises, Items),
    premisesListed(Items, List).

%! ruleParts(+Rule, -Id, -Premises, -Conclusion)
%  The parts of Rule, a rule or a premise as written: [Id, Premises, Conclusion] (Premises read as premiseList/2 reads
%  them) or [Id, Conclusion].
ruleParts([Id, Premises, Conclusion], Id, List, Conclusion) :-
    premiseList(Premises, List).
ruleParts([Id, Conclusion], Id, [], Conclusion).

% The items of the premises of a rule as the list of its premises: none for [], X alone for [X], written as a list
premisesListed([[]], []) :- !.
premisesListed([[X]], [X]) :- !.
premisesListed(X, X).

%! rule(?Id, ?Premises, ?Conclusion)
%  The rule Id concludes Conclusion ([Statement]) from Premises (a list); with metaRules, applicable(Id) is its
%  first premise.
rule(X, [applicable(X)|Y], Z) :- setting(metaRules), !, ruleWritten(X, Y, Z).
rule(X, Y, Z) :- ruleWritten(X, Y, Z).

%! ruleUsing(?Premise, ?Conclusion)
%  A rule concludes Conclusion (a statement) from Premise, among others: rule/3 and member/2 over its
%  premises, through the index of the rules built once in the active context.
ruleUsing(Premise, Conclusion) :-
    rulesIndexed,
    context_check(premiseOf(Premise, Conclusion, _)).

%! ruleUsingAt(?Premise, ?Place)
%  The rule at Place (see ruleAt/2) has Premise among its premises.
ruleUsingAt(Premise, Place) :-
    rulesIndexed,
    context_check(premiseOf(Premise, _, Place)).

%! ruleAt(?Place, ?Rule)
%  Rule, [Id, Premises, Conclusion], is the one at Place in the order of rule/3.
ruleAt(Place, Rule) :-
    rulesPlaced,
    context_check(ruleAt(Place, Rule)).

% The index: premiseOf/3 facts asserted from the last, as context_check returns the newest clauses first, so
% that lookups give the rules in the order of rule/3
rulesIndexed :- context_check(rulesIndexed), !.
rulesIndexed :-
    findall(premiseOf(P, C, Place), (placedRule(Place, [_, Premises, [C]]), member(P, Premises)), Index),
    assertFromLast(Index),
    context_assert(rulesIndexed).

%! ruleConclusion(?Id, ?Conclusion)
%  The rule with identifier Id concludes Conclusion: looked up by its identifier, not among all the rules.
ruleConclusion(Id, Conclusion) :-
    conclusionsIndexed,
    context_check(ruleIdOf(Id, Conclusion)).

conclusionsIndexed :- context_check(conclusionsIndexed), !.
conclusionsIndexed :-
    findall(_, (rule(Id, _, [Conclusion]), context_assert(ruleIdOf(Id, Conclusion))), _),
    context_assert(conclusionsIndexed).

% The rules themselves, only for who reads them by their places
rulesPlaced :- context_check(rulesPlaced), !.
rulesPlaced :-
    findall(_, (placedRule(Place, Rule), context_assert(ruleAt(Place, Rule))), _),
    context_assert(rulesPlaced).

placedRule(Place, Rule) :-
    findall([Id, Premises, Conclusion], rule(Id, Premises, Conclusion), Rules),
    placed(Rules, 0, Place, Rule).

placed([Rule|_], Place, Place, Rule).
placed([_|Rules], At, Place, Rule) :-
    Next is At + 1,
    placed(Rules, Next, Place, Rule).

assertFromLast([]).
assertFromLast([Fact|Facts]) :-
    assertFromLast(Facts),
    context_assert(Fact).

%==============================================================================
% The clauses of the rules and premises read, in the cache (see RuleTerm), and the premises of the rules
%==============================================================================

%! ruleClause(?Id, ?Premises, ?Conclusion, ?Head, ?Body)
%  Head :- Body is the clause of the rule Id, concluding Conclusion from Premises (a conjunction).
ruleClause(Id, Premises, Conclusion, rl(Conclusion), [Id, Premises, Conclusion]).

%! premiseClause(?Id, ?Conclusion, ?Head, ?Body)
%  Head :- Body is the clause of the premise Id, concluding Conclusion.
premiseClause(Id, Conclusion, rl(Conclusion), [Id, Conclusion]).

%! clauseHead(?Conclusion, ?Head)
%  Head is the head of the clauses of the rules and premises concluding Conclusion.
clauseHead(Conclusion, rl(Conclusion)).

%! premiseKind(?Premise, ?Kind)
%  What the premise Premise of a rule is: contrary(Statement) for ~(Statement), recorded for the attacks on the
%  argument (an argument for Statement attacks it); check(Goal) for prolog(Goal), a Prolog goal that holds or not;
%  constraint(Constraint) for { Constraint } (see the module constraints); a statement, supported by an argument,
%  otherwise (a variable as well).
premiseKind(Premise, Kind) :- var(Premise), !, Kind = statement.
premiseKind(~(Statement), Kind) :- !, Kind = contrary(Statement).
premiseKind(prolog(Goal), Kind) :- !, Kind = check(Goal).
premiseKind('{}'(Constraint), Kind) :- !, Kind = constraint(Constraint).
premiseKind(_, statement).

%! conclusionOf(?Id, ?Conclusion)
%  The rule or premise Id concludes Conclusion (a statement): the rules looked up by their identifier.
conclusionOf(Id, Conclusion) :- ruleConclusion(Id, Conclusion).
conclusionOf(Id, Conclusion) :- premise(Id, [Conclusion]).
