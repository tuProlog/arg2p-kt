%! concludable(+Conclusion)
%  An argument may conclude Conclusion: ground, or open (with free variables, constrained or not) with the graph
%  extension constraints (or temporal), for the values its variables may take.
concludable(Conclusion) :- ground(Conclusion), !.
concludable(_) :- instances.

%! constraintsOf(+Argument, -Constraints)
%  Constraints are those of Argument: its { } premises, those of its sub-arguments still open included.
constraintsOf(Argument, Constraints) :-
    store::body(Argument, Body),
    constraints(Body, Constraints).

constraints([], []).
constraints([Premise|Premises], [C|Cs]) :- parser::premiseKind(Premise, constraint(C)), !, constraints(Premises, Cs).
constraints([_|Premises], Cs) :- constraints(Premises, Cs).

%! extend(+Body, +Supports, -Extended)
%  Body, the premises of a rule supported by the arguments Supports, extended with the constraints of Supports
%  still open: the constraints of Body and of Supports may all hold together.
extend(Body, Supports, Extended) :-
    constraints(Body, Own),
    inherited(Supports, Inherited),
    (Own == [], Inherited == [] ->
        Extended = Body
    ;
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

%! compatible(+A, +B)
%  The constraints of A and B, with the bindings they have (those of a conflict between them), may hold together.
compatible(A, B) :-
    constraintsOf(A, SA),
    constraintsOf(B, SB),
    append(SA, SB, All),
    satisfiable(All).

%! satisfiable(+Constraints)
%  Constraints may hold together: the ground ones hold, and each variable has a value satisfying those on it alone.
satisfiable([]) :- !.
satisfiable(Constraints) :-
    conjuncts(Constraints, All),
    holding(All),
    normals(All, Normals),
    consistent(Normals).

conjuncts([], []).
conjuncts([C|Cs], All) :- parser::tupleToList(C, All, Rest), conjuncts(Cs, Rest).

holding([]).
holding([C|Cs]) :- (ground(C) -> constraintHolds(C) ; true), holding(Cs).

% A ground constraint holds (any other goal is checked as by prolog/1)
constraintHolds(member(X, List)) :- !, member(X, List).
constraintHolds(C) :- comparison(C), !, catch(C, _, fail).
constraintHolds(X = Y) :- !, X = Y.
constraintHolds(X \= Y) :- !, X \= Y.
constraintHolds(X == Y) :- !, X == Y.
constraintHolds(X \== Y) :- !, X \== Y.
constraintHolds(C) :- user::call(C).

comparison(C) :- C =.. [Op, _, _], member(Op, ['<', '=<', '>', '>=', '=:=', '=\\=']).

% The open constraints on a single variable, normalised: set(X, Values), out(X, Term), bound(X, Op, Number);
% the others wait for their variables
normals([], []).
normals([C|Cs], [N|Ns]) :- \+ ground(C), normal(C, N), !, normals(Cs, Ns).
normals([_|Cs], Ns) :- normals(Cs, Ns).

normal(member(X, List), set(X, List)) :- var(X), ground(List).
normal(X = T, set(X, [T])) :- var(X), ground(T).
normal(T = X, set(X, [T])) :- var(X), ground(T).
normal(X \= T, out(X, T)) :- var(X), ground(T).
normal(T \= X, out(X, T)) :- var(X), ground(T).
normal(X \== T, out(X, T)) :- var(X), ground(T).
normal(T \== X, out(X, T)) :- var(X), ground(T).
normal(C, bound(X, Op, V)) :-
    comparison(C), C =.. [Op, X, E], var(X), ground(E), catch(V is E, _, fail).
normal(C, bound(X, Flipped, V)) :-
    comparison(C), C =.. [Op, E, X], var(X), ground(E), catch(V is E, _, fail), flipped(Op, Flipped).

flipped('<', '>').
flipped('>', '<').
flipped('=<', '>=').
flipped('>=', '=<').
flipped('=:=', '=:=').
flipped('=\\=', '=\\=').

% Each variable has a value: the constraints grouped by their variable
consistent([]).
consistent([N|Ns]) :-
    subject(N, X),
    split(Ns, X, Same, Others),
    domain([N|Same]),
    consistent(Others).

subject(set(X, _), X).
subject(out(X, _), X).
subject(bound(X, _, _), X).

split([], _, [], []).
split([N|Ns], X, [N|Same], Others) :- subject(N, Y), Y == X, !, split(Ns, X, Same, Others).
split([N|Ns], X, Same, [N|Others]) :- split(Ns, X, Same, Others).

% A value satisfies all the constraints of Group: one of a set, a point, or one of a dense interval (whose
% exclusions, finitely many, leave others)
domain(Group) :- member(set(_, Values), Group), !, member(V, Values), allows(Group, V), !.
domain(Group) :- member(bound(_, '=:=', P), Group), !, allows(Group, P).
domain(Group) :-
    lower(Group, none, Low),
    upper(Group, none, High),
    nonEmpty(Low, High, Group).

allows(Group, V) :- \+ (member(N, Group), \+ admits(N, V)).

admits(set(_, Values), V) :- member(V, Values).
admits(out(_, T), V) :- V \= T.
admits(bound(_, Op, B), V) :- number(V), C =.. [Op, V, B], call(C).

lower([], Low, Low).
lower([bound(_, Op, B)|Ns], Acc, Low) :- lowerKind(Op, Kind), !, higher(Acc, lo(B, Kind), Next), lower(Ns, Next, Low).
lower([_|Ns], Acc, Low) :- lower(Ns, Acc, Low).

lowerKind('>', open).
lowerKind('>=', closed).

higher(none, L, L) :- !.
higher(lo(A, KA), lo(B, KB), L) :-
    (B > A -> L = lo(B, KB) ; B < A -> L = lo(A, KA) ; KA == open -> L = lo(A, open) ; L = lo(B, KB)).

upper([], High, High).
upper([bound(_, Op, B)|Ns], Acc, High) :- upperKind(Op, Kind), !, lesser(Acc, hi(B, Kind), Next), upper(Ns, Next, High).
upper([_|Ns], Acc, High) :- upper(Ns, Acc, High).

upperKind('<', open).
upperKind('=<', closed).

lesser(none, H, H) :- !.
lesser(hi(A, KA), hi(B, KB), H) :-
    (B < A -> H = hi(B, KB) ; B > A -> H = hi(A, KA) ; KA == open -> H = hi(A, open) ; H = hi(B, KB)).

nonEmpty(none, _, _) :- !.
nonEmpty(_, none, _) :- !.
nonEmpty(lo(A, _), hi(B, _), _) :- A < B, !.
nonEmpty(lo(A, closed), hi(B, closed), Group) :- A =:= B, allows(Group, A).

%! instances
%  The arguments may hold for sets of values, and get their instances.
instances :-
    utils::oncePerTheory(instances, constraints::readInstances),
    cache_check(theoryKind(instances, yes)).

readInstances :-
    (open -> Kind = yes ; Kind = no),
    cache_assert(theoryKind(instances, Kind)).

open :- setting(graphExtension, constraints), !.
open :- parser::extension(Extension), module_defines(Extension, opensConclusions/0), !.
open :- setting(openConclusions).

%! constrained
%  An argument may have an open conclusion, or a premise of a rule is a constraint. Otherwise every argument concludes a
%  ground statement.
constrained :-
    utils::oncePerTheory(constrained, constraints::readConstrained),
    cache_check(theoryKind(constrained, yes)).

readConstrained :- \+ \+ readConstrained(_).

readConstrained(Rules) :-
    (instances -> Kind = yes ; parser::writtenRules(Rules), constrainedRules(Rules, Kind)),
    cache_assert(theoryKind(constrained, Kind)).

%! constrainedRules(+Rules, -Constrained)
%  Constrained is yes if a premise of Rules, as written, is a constraint; no otherwise.
constrainedRules(Rules, yes) :-
    member(Rule, Rules),
    parser::ruleParts(Rule, _, Premises, _),
    member(Premise, Premises),
    parser::premiseKind(Premise, constraint(_)), !.
constrainedRules(_, no).
