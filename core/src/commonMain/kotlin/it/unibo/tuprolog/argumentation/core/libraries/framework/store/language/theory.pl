%==============================================================================
% THE RULES AND PREMISES OF THE THEORY: read by the parser the first time they are asked for (see parser::readTheory/2),
% kept in the cache as clauses (see RuleTerm): `rl(Conclusion) :- [Id, Premises, Conclusion]` for a rule, Premises a
% conjunction, `rl(Conclusion) :- [Id, Conclusion]` for a premise (ruleClause/5, premiseClause/4), and strict/1 facts.
%==============================================================================

%! rulesRead
%  The rules and premises of the theory read and kept, once for the theory (see utils::oncePerTheory/2).
rulesRead :- utils::oncePerTheory(rules, store::readRules).

%! readRules
%  What the parser reads of the theory, kept in the cache as clauses (see ruleToClause/2) and strict/1 facts. The term as
%  large as the theory is not kept bound: it weighs on every binding made after it.
readRules :- \+ \+ readRules(_).

readRules(Stored) :-
    parser::readTheory(Rules, StrictIds),
    ruleToClause(Rules, Clauses),
    utils::appendLists([Clauses, StrictIds], Stored),
    utils::cacheAll(Stored),
    (constrainedRules(Rules) -> cacheAssert(constrainedTheory) ; true), !.

%! constrained
%  A premise of a rule of the theory is a constraint ({ C }, see premiseKind/2): found as the rules are read.
constrained :-
    rulesRead,
    cacheCheck(constrainedTheory).

constrainedRules(Rules) :-
    member(Rule, Rules),
    ruleParts(Rule, _, Premises, _),
    member(P, Premises),
    premiseKind(P, constraint(_)), !.

%! strictness(+Id, -Strictness)
%  The rule Id is strict or defeasible (see checkStrict/1).
strictness(Id, strict) :- checkStrict(Id), !.
strictness(_, defeasible).

%! rule(?Id, ?Premises, ?Conclusion)
%  The rule Id concludes Conclusion ([Statement]) from Premises (a list), as read (see parser::readTheory/2).
rule(Id, PP, [Conclusion]) :-
    rulesRead,
    ruleClause(Id, Premises, Conclusion, Head, Body),
    cacheCheck(clause(Head, Body)),
    premiseList(Premises, PP).

%! premise(?Id, ?Conclusion)
%  The premise or axiom Id concludes Conclusion ([Statement]).
premise(Id, [Conclusion]) :-
    rulesRead,
    premiseClause(Id, Conclusion, Head, Body),
    cacheCheck(clause(Head, Body)).

%! writtenRules(-Rules)
%  The rules and premises as they are read: [Id, Premises, Conclusion] or [Id, Conclusion] (see ruleParts/4).
writtenRules(Rules) :-
    rulesRead,
    clauseHead(_, Head),
    findall(Body, cacheCheck(clause(Head, Body)), Rules).

%! ruleClauses(-Clauses)
%  The clauses of the rules and premises, as this store keeps them.
ruleClauses(Clauses) :-
    rulesRead,
    clauseHead(_, Head),
    findall((Head :- Body), cacheCheck(clause(Head, Body)), Clauses).

%! countConcluding(+Statement, +Limit, -Count)
%  The number of rules and premises concluding Statement, counted up to Limit (an integer, or anything else for no limit)
countConcluding(Statement, Limit, Count) :-
    rulesRead,
    clauseHead(Statement, Head),
    cacheCount(clause(Head, _), Limit, Count).

%! checkStrict(?Id)
%  The rule or premise Id is strict.
checkStrict(Id) :- rulesRead, cacheCheck(strict(Id)).

%! premiseList(+Premises, -List)
%  The premises of a rule, as read (a conjunction), as a list.
premiseList(Premises, List) :-
    utils::tupleToList(Premises, Items),
    premisesListed(Items, List).

%! ruleParts(+Rule, -Id, -Premises, -Conclusion)
%  The parts of Rule, a rule or a premise as read: [Id, Premises, Conclusion] (Premises read as premiseList/2 reads them) or
%  [Id, Conclusion].
ruleParts([Id, Premises, Conclusion], Id, List, Conclusion) :-
    premiseList(Premises, List).
ruleParts([Id, Conclusion], Id, [], Conclusion).

% The items of the premises of a rule as the list of its premises: none for [], X alone for [X], written as a list
premisesListed([[]], []) :- !.
premisesListed([[X]], [X]) :- !.
premisesListed(X, X).

%! ruleUsing(?Premise, ?Conclusion)
%  A rule concludes Conclusion (a statement) from Premise, among others: rule/3 and member/2 over its premises, through the
%  index of the rules built once in the active context.
ruleUsing(Premise, Conclusion) :-
    rulesIndexed,
    contextCheck(premiseOf(Premise, Conclusion, _)).

%! ruleUsingAt(?Premise, ?Place)
%  The rule at Place (see ruleAt/2) has Premise among its premises.
ruleUsingAt(Premise, Place) :-
    rulesIndexed,
    contextCheck(premiseOf(Premise, _, Place)).

%! ruleAt(?Place, ?Rule)
%  Rule, [Id, Premises, Conclusion], is the one at Place in the order of rule/3.
ruleAt(Place, Rule) :-
    rulesPlaced,
    contextCheck(ruleAt(Place, Rule)).

% The index: premiseOf/3 facts asserted from the last, as contextCheck returns the newest clauses first, so that lookups
% give the rules in the order of rule/3
rulesIndexed :- contextCheck(rulesIndexed), !.
rulesIndexed :-
    findall(premiseOf(P, C, Place), (placedRule(Place, [_, Premises, [C]]), member(P, Premises)), Index),
    assertFromLast(Index),
    contextAssert(rulesIndexed).

%! ruleConclusion(?Id, ?Conclusion)
%  The rule with identifier Id concludes Conclusion: looked up by its identifier, not among all the rules.
ruleConclusion(Id, Conclusion) :-
    conclusionsIndexed,
    contextCheck(ruleIdOf(Id, Conclusion)).

conclusionsIndexed :- contextCheck(conclusionsIndexed), !.
conclusionsIndexed :-
    findall(_, (rule(Id, _, [Conclusion]), contextAssert(ruleIdOf(Id, Conclusion))), _),
    contextAssert(conclusionsIndexed).

% The rules themselves, only for who reads them by their places
rulesPlaced :- contextCheck(rulesPlaced), !.
rulesPlaced :-
    findall(_, (placedRule(Place, Rule), contextAssert(ruleAt(Place, Rule))), _),
    contextAssert(rulesPlaced).

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
    contextAssert(Fact).

%! conclusionOf(?Id, ?Conclusion)
%  The rule or premise Id concludes Conclusion (a statement): the rules looked up by their identifier.
conclusionOf(Id, Conclusion) :- ruleConclusion(Id, Conclusion).
conclusionOf(Id, Conclusion) :- premise(Id, [Conclusion]).

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
%  What the premise Premise of a rule is: contrary(Statement) for ~(Statement), recorded for the attacks on the argument
%  (an argument for Statement attacks it); check(Goal) for prolog(Goal), a Prolog goal that holds or not (see
%  parser::holds/1); constraint(Constraint) for { Constraint } (see the module constraints); a statement, supported by an
%  argument, otherwise (a variable as well).
premiseKind(Premise, Kind) :- var(Premise), !, Kind = statement.
premiseKind(~(Statement), Kind) :- !, Kind = contrary(Statement).
premiseKind(prolog(Goal), Kind) :- !, Kind = check(Goal).
premiseKind('{}'(Constraint), Kind) :- !, Kind = constraint(Constraint).
premiseKind(_, statement).

%! constraintsHold(+Body)
%  The constraints of Body, the premises of a rule ({ C }, see premiseKind/2), as checks: each ground, holding.
constraintsHold([]).
constraintsHold([Premise|Premises]) :-
    (premiseKind(Premise, constraint(C)) -> ground(C), parser::holds(C) ; true),
    constraintsHold(Premises).

%==============================================================================
% THE STATEMENT OF AN ARGUMENT
%==============================================================================

%! statement(+Argument, -Statement)
%  Statement is the statement Argument stands for: its conclusion, [Claim], or [Claim, {C}] for an argument holding for the
%  values its open constraints C allow (see premiseKind/2), as the labelled statements are written.
statement(Argument, Statement) :-
    conclusion(Argument, Conclusion),
    body(Argument, Body),
    ground(Body), !,
    Statement = Conclusion.
statement(Argument, Statement) :-
    conclusion(Argument, Conclusion),
    body(Argument, Body),
    openConstraints(Body, Open),
    Conclusion = [Claim],
    utils::termVariables(Claim, Vars),
    connected(Open, Vars, Connected),
    flattened(Connected, [], Constraints),
    (Constraints == [] -> Statement = Conclusion ; conjunction(Constraints, C), Statement = [Claim, '{}'(C)]).

% The constraints, their conjunctions taken apart, each once (the same term), in their order
flattened([], Kept, Kept).
flattened([C|Cs], Kept, All) :-
    utils::tupleToList(C, Parts),
    keptOnce(Parts, Kept, More),
    flattened(Cs, More, All).

keptOnce([], Kept, Kept).
keptOnce([P|Ps], Kept, All) :- member(K, Kept), K == P, !, keptOnce(Ps, Kept, All).
keptOnce([P|Ps], Kept, All) :- append(Kept, [P], More), keptOnce(Ps, More, All).

% The constraints on the variables of the claim, or on those of the constraints so kept: the others only say that some values
% satisfy them, as the argument does by being
connected(Constraints, Vars, Kept) :-
    connectedPass(Constraints, Vars, Taken, Left, More),
    (Taken == [] -> Kept = [] ; connected(Left, More, Rest), append(Taken, Rest, Kept)).

connectedPass([], Vars, [], [], Vars).
connectedPass([C|Cs], Vars, Taken, Left, More) :-
    utils::termVariables(C, CVars),
    (sharesVariable(CVars, Vars) ->
        Taken = [C|T], append(Vars, CVars, Wider), connectedPass(Cs, Wider, T, Left, More)
    ;
        Left = [C|L], connectedPass(Cs, Vars, Taken, L, More)
    ).

sharesVariable(CVars, Vars) :- member(V, CVars), member(W, Vars), V == W, !.

openConstraints([], []).
openConstraints([P|Ps], [C|Cs]) :- premiseKind(P, constraint(C)), \+ ground(C), !, openConstraints(Ps, Cs).
openConstraints([_|Ps], Cs) :- openConstraints(Ps, Cs).

conjunction([C], C) :- !.
conjunction([C|Cs], (C, Rest)) :- conjunction(Cs, Rest).
