%==============================================================================
% HOOKS: what the base modules call where this extension is one of the theory's (see parser::hook/2): the graph
% builders (concludable/1, argumentBody/3, compatible/2), the exploration of a query (explored/2), the answers to it
% (admitted/3)
%==============================================================================

%! concludable(+Conclusion)
%  An argument may conclude Conclusion: ground, or open (with free variables, constrained or not), for the values its
%  variables may take.
concludable(Conclusion) :- ground(Conclusion), !.
concludable(_) :- instances.

%! explored(+Statement, -Explored)
%  Statement is explored in query mode as Explored, its arguments free where an argument may leave them open (a negation
%  kept, see openPosition/2): the whole graph has the open arguments for it, and those for the values asked are their
%  instances and their pieces, which the graph step adds as in the whole graph. The other arguments are kept: no
%  argument has another value there (holdsAt(f, 5) is explored as holdsAt(f, T), the periods of f alone).
explored(S, S) :- var(S), !.
explored(-(S), -(G)) :- !, explored(S, G).
explored(S, G) :-
    functor(S, Name, Arity),
    functor(G, Name, Arity),
    S =.. [_|Args],
    G =.. [_|Free],
    keptArguments(Args, Free, 1, Name/Arity).

keptArguments([], [], _, _).
keptArguments([A|As], [F|Fs], I, Key) :-
    (openPosition(Key, I) -> true ; F = A),
    J is I + 1,
    keptArguments(As, Fs, J, Key).

%! openPosition(?Key, ?Position)
%  An argument for a statement Key (Name/Arity, a negation left out) may leave its argument at Position open: the
%  conclusion of a premise or a rule has a variable there that its statement premises do not bind (a variable of a
%  constraint alone, { T > T1 }, or of a premise at a position that is open in turn), a Prolog check binding its
%  variables (it computes them, as X is Expr or conflict::between/2, or needs them bound). Read the first time it is asked for, the open positions found round after round until
%  no new one is.
openPosition(Key, Position) :-
    utils::oncePerTheory(openPositions, constraints::readOpenPositions),
    cacheCheck(openPositionOf(Key, Position)).

readOpenPositions :-
    findall(K-I, (
        ruleHead(Premises, Head),
        copy_term(Premises-Head, P-H),
        openIn(P, H, K, I),
        \+ cacheCheck(openPositionOf(K, I))
    ), Found),
    utils::deduplicate(Found, New),
    New \== [], !,
    findall(_, (member(K-I, New), cacheAssert(openPositionOf(K, I))), _),
    readOpenPositions.
readOpenPositions.

ruleHead(Premises, Head) :- store::rule(_, Premises, [Head]).
ruleHead([], Head) :- store::premise(_, [Head]).

% openIn(+Premises, +Head, -Key, -Position): Head has an argument at Position its Premises leave unbound
openIn(Premises, Head, Key, Position) :-
    nonvar(Head),
    positive(Head, H),
    compound(H),
    bindPremises(Premises),
    H =.. [Name|Args],
    length(Args, Arity),
    Key = Name/Arity,
    argumentAt(Args, 1, Position, A),
    \+ ground(A).

positive(-(S), S) :- !.
positive(S, S).

argumentAt([A|_], I, I, A).
argumentAt([_|As], I, Position, A) :- J is I + 1, argumentAt(As, J, Position, A).

% The variables the premises bind, in their order: a statement those at its positions not open, a check all of its
% own; a constraint or a contrary nothing
bindPremises([]).
bindPremises([P|Ps]) :-
    store::premiseKind(P, Kind),
    bindAs(Kind, P),
    bindPremises(Ps).

bindAs(statement, P) :- !, (var(P) -> true ; positive(P, S), bindStatement(S)).
bindAs(check(Goal), _) :- !, bindAll(Goal).
bindAs(_, _).

bindStatement(S) :- compound(S), !, S =.. [Name|Args], length(Args, Arity), bindKept(Args, 1, Name/Arity).
bindStatement(_).

bindKept([], _, _).
bindKept([A|As], I, Key) :-
    (cacheCheck(openPositionOf(Key, I)) -> true ; bindAll(A)),
    J is I + 1,
    bindKept(As, J, Key).

bindAll(Term) :- var(Term), !, Term = bound.
bindAll(Term) :- atomic(Term), !.
bindAll(Term) :- Term =.. [_|Args], bindEach(Args).
bindEach([]).
bindEach([A|As]) :- bindAll(A), bindEach(As).

%! admitted(+Statement, +Goal, -Answer)
%  Statement, as labelled (see store::statement/2), holds for values Goal may take: its claim unifies with Goal, and its
%  constraints may hold with the bindings. Answer is Goal so bound, with the constraints still open on it as
%  [Goal, {C}].
admitted([Claim], Goal, Goal) :-
    Claim = Goal.
admitted([Claim, '{}'(C)], Goal, Answer) :-
    Claim = Goal,
    satisfiable([C]),
    (ground(C) -> Answer = Goal ; Answer = [Goal, '{}'(C)]).

%! constraintsOf(+Argument, -Constraints)
%  Constraints are those of Argument: its { } premises, those of its sub-arguments still open included.
constraintsOf(Argument, Constraints) :-
    store::body(Argument, Body),
    constraints(Body, Constraints).

constraints([], []).
constraints([Premise|Premises], [C|Cs]) :- store::premiseKind(Premise, constraint(C)), !, constraints(Premises, Cs).
constraints([_|Premises], Cs) :- constraints(Premises, Cs).

%! argumentBody(+Body, +Supports, -Extended)
%  Body, the premises of a rule supported by the arguments Supports, extended with the constraints of Supports
%  still open: the constraints of the rule outside those decided (see decided/1) checked as prolog/1 checks are, in
%  their order, then those of Body and of Supports may all hold together.
argumentBody(Body, Supports, Extended) :-
    constraints(Body, Own),
    inherited(Supports, Inherited),
    (Own == [], Inherited == [] ->
        Extended = Body
    ;
        conjuncts(Own, OwnConjuncts),
        checked(OwnConjuncts),
        append(Own, Inherited, All),
        satisfiable(All),
        open(Inherited, Open),
        append(Body, Open, Extended)
    ).

inherited([], []).
inherited([Support|Supports], Constraints) :-
    constraintsOf(Support, Own),
    inherited(Supports, Others),
    append(Own, Others, Constraints).

% The constraints still open, as premises { C }
open([], []).
open([C|Cs], ['{}'(C)|Os]) :- \+ ground(C), !, open(Cs, Os).
open([_|Cs], Os) :- open(Cs, Os).

% The constraints of a rule the module does not decide, run as prolog/1 runs its goal (binding what they bind)
checked([]).
checked([C|Cs]) :- (decided(C) -> true ; parser::holds(C)), checked(Cs).

%! compatible(+A, +B)
%  The constraints of A and B, with the bindings they have (those of a conflict between them), may hold together.
compatible(A, B) :-
    constraintsOf(A, SA),
    constraintsOf(B, SB),
    append(SA, SB, All),
    satisfiable(All).

%! decided(+Constraint)
%  The module decides Constraint whatever its variables: a comparison of linear expressions over numbers (dense:
%  linearConstraint/1), or dif(X, T), X not T, a term other than a number (the pieces of an argument where a
%  conflict binds its variables by unification, see the graph step). Any other constraint is a check.
decided(C) :- linearConstraint(C), !.
decided(dif(_, _)).

%! satisfiable(+Constraints)
%  Constraints may hold together: the linear ones, by the simplex over the rationals (linearSatisfiable/1); dif(X, T)
%  unless X and T are the same term; any other, once ground, holding (open, it was run where its rule was used).
satisfiable([]) :- !.
satisfiable(Constraints) :-
    conjuncts(Constraints, All),
    linears(All, Linear),
    linearSatisfiable(Linear).

conjuncts([], []).
conjuncts([C|Cs], All) :- utils::tupleToList(C, All, Rest), conjuncts(Cs, Rest).

linears([], []).
linears([C|Cs], [C|Ls]) :- linearConstraint(C), !, linears(Cs, Ls).
linears([dif(X, T)|Cs], Ls) :- !, X \== T, linears(Cs, Ls).
linears([C|Cs], Ls) :- (ground(C) -> parser::holds(C) ; true), linears(Cs, Ls).

%! instances
%  The arguments may hold for sets of values, and get their instances and pieces: the extension is one of the theory's
%  (see parser::extension/1), stated, required by another (temporal), or set by its flag (openConclusions).
instances :-
    utils::oncePerTheory(instances, constraints::readInstances),
    cacheCheck(theoryKind(instances, yes)).

readInstances :-
    (parser::extension(constraints) -> Kind = yes ; Kind = no),
    cacheAssert(theoryKind(instances, Kind)).
