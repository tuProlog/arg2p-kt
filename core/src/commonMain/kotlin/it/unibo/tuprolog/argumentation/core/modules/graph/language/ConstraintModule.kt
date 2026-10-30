package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.Subsumes
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The constraints on the variables of the arguments (module `constraint`): a premise `{ C }` of a rule is checked as
 * `prolog(C)` once its variables are bound, and kept on the argument while they are not, so that an argument may hold
 * for a set of values (`r : started(F, T1), { T > T1 } => holdsAt(F, T)` concludes `holdsAt(f, T)` for every T after
 * T1). An argument keeps the constraints of its sub-arguments still open among its premises (its body), and a premise
 * or a conflict binding its variables is admitted only while all of them may hold together.
 *
 * The constraints decided are those on a single variable: comparisons with a number (`<`, `=<`, `>`, `>=`, `=:=`,
 * `=\=`, the numbers dense), membership (`member(X, [a, b])`), equality and exclusion of a term (`X = a`, `X \= a`), and
 * their conjunctions (`{ X > 1, X < 5 }`). A constraint relating two variables waits for one of them to be bound.
 */
object ConstraintModule : PrologModule("prolog.argumentation.language.constraint", "constraint") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(Subsumes::descriptionPair.get())

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! concludable(+Conclusion)
        %  An argument may conclude Conclusion: ground, or open (with free variables, constrained or not) with the graph
        %  extension constraints (or temporal), for the values its variables may take.
        concludable(Conclusion) :- ground(Conclusion), !.
        concludable(_) :- parser::instances.

        %! premise(?Premise, ?Constraint)
        %  Premise, of a rule, is the constraint Constraint: { Constraint }.
        premise(Premise, Constraint) :- nonvar(Premise), Premise = '{}'(Constraint).

        %! store(+Argument, -Constraints)
        %  Constraints are those of Argument: its { } premises, those of its sub-arguments still open included.
        store(Argument, Constraints) :-
            argument::body(Argument, Body),
            constraints(Body, Constraints).

        constraints([], []).
        constraints([Premise|Premises], [C|Cs]) :- premise(Premise, C), !, constraints(Premises, Cs).
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
                join(Own, Inherited, All),
                satisfiable(All),
                open(Inherited, Open),
                join(Body, Open, Extended)
            ).

        inherited([], []).
        inherited([Support|Supports], Constraints) :-
            store(Support, Own),
            inherited(Supports, Others),
            join(Own, Others, Constraints).

        % The constraints still open, as premises { C }
        open([], []).
        open([C|Cs], ['{}'(C)|Os]) :- \+ ground(C), !, open(Cs, Os).
        open([_|Cs], Os) :- open(Cs, Os).

        %! admitted(+Argument)
        %  The constraints of Argument, with the bindings it has, may hold together.
        admitted(Argument) :-
            store(Argument, Constraints),
            satisfiable(Constraints).

        %! compatible(+A, +B)
        %  The constraints of A and B, with the bindings they have (those of a conflict between them), may hold together.
        compatible(A, B) :-
            store(A, SA),
            store(B, SB),
            join(SA, SB, All),
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
        conjuncts([C|Cs], All) :- conjunct(C, All, Rest), conjuncts(Cs, Rest).

        conjunct(C, [C|Rest], Rest) :- var(C), !.
        conjunct((A, B), All, Rest) :- !, conjunct(A, All, Middle), conjunct(B, Middle, Rest).
        conjunct(C, [C|Rest], Rest).

        holding([]).
        holding([C|Cs]) :- (ground(C) -> holds(C) ; true), holding(Cs).

        % A ground constraint holds (any other goal is checked as by prolog/1)
        holds(member(X, List)) :- !, member(X, List).
        holds(C) :- comparison(C), !, catch(C, _, fail).
        holds(X = Y) :- !, X = Y.
        holds(X \= Y) :- !, X \= Y.
        holds(X == Y) :- !, X == Y.
        holds(X \== Y) :- !, X \== Y.
        holds(C) :- user::call(C).

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

        join([], L, L).
        join([X|Xs], L, [X|Ys]) :- join(Xs, L, Ys).
        """.trimIndent()
}
