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
%  Statement is explored in query mode as Explored, with its arguments free (a negation kept): the whole graph has the
%  arguments for it, and those for the values asked are their instances and their pieces, which the graph step adds as
%  in the whole graph.
explored(S, S) :- var(S), !.
explored(-(S), -(G)) :- !, explored(S, G).
explored(S, G) :- functor(S, Name, Arity), functor(G, Name, Arity).

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
%  linear_constraint/1), or dif(X, T), X not T, a term other than a number (the pieces of an argument where a
%  conflict binds its variables by unification, see the graph step). Any other constraint is a check.
decided(C) :- linear_constraint(C), !.
decided(dif(_, _)).

%! satisfiable(+Constraints)
%  Constraints may hold together: the linear ones, by the simplex over the rationals (linear_satisfiable/1); dif(X, T)
%  unless X and T are the same term; any other, once ground, holding (open, it was run where its rule was used).
satisfiable([]) :- !.
satisfiable(Constraints) :-
    conjuncts(Constraints, All),
    linears(All, Linear),
    linear_satisfiable(Linear).

conjuncts([], []).
conjuncts([C|Cs], All) :- utils::tupleToList(C, All, Rest), conjuncts(Cs, Rest).

linears([], []).
linears([C|Cs], [C|Ls]) :- linear_constraint(C), !, linears(Cs, Ls).
linears([dif(X, T)|Cs], Ls) :- !, X \== T, linears(Cs, Ls).
linears([C|Cs], Ls) :- (ground(C) -> parser::holds(C) ; true), linears(Cs, Ls).

%! instances
%  The arguments may hold for sets of values, and get their instances and pieces: the extension is one of the theory's
%  (see parser::extension/1), stated, required by another (temporal), or set by its flag (openConclusions).
instances :-
    utils::oncePerTheory(instances, constraints::readInstances),
    cache_check(theoryKind(instances, yes)).

readInstances :-
    (parser::extension(constraints) -> Kind = yes ; Kind = no),
    cache_assert(theoryKind(instances, Kind)).
