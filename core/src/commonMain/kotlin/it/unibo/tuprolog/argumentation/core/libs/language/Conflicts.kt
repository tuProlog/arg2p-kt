package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The conflicts between statements (module `conflict`). Without metaConflicts they are the built-in ones (a literal
 * and its negation, and those between obligations and permissions) and those the theory declares (conflict/2,3);
 * with metaConflicts they are only those concluded by an argument for `conflict(A, B)` or `conflict(A, B, Guard)`.
 *
 * Statements are written as in the conclusions of arguments, `[Claim]`. A conflict may hold only under a guard, a
 * Prolog goal (or a term true or false) checked when the conflict is used.
 */
object Conflicts : PrologModule("prolog.argumentation.language.conflict", "conflict") {
    override val prologRawTheory: String =
        """
        %! between(?Statement, ?Other)
        %  Statement conflicts with Other, the guard of the conflict holding.
        between(Statement, Other) :-
            unguarded(Statement, Other).
        between(Statement, Other) :-
            guarded(Statement, Other, Guard),
            holds(Guard).

        %! stated(+Conflict, ?Statement, ?Other)
        %  Conflict, a conflict(A, B) or conflict(A, B, Guard) term, makes Statement ([A]) conflict with Other ([B]),
        %  its guard holding.
        stated(conflict(A, B), [A], [B]).
        stated(conflict(A, B, Guard), [A], [B]) :-
            holds(Guard).

        %! guarded(?Statement, ?Other, -Guard)
        %  Statement conflicts with Other when Guard holds.
        guarded(Statement, Other, Guard) :- setting(metaConflicts), !,
            Statement = [A], Other = [B],
            graphStore::argumentFor([conflict(A, B, Guard)], _).
        guarded(Statement, Other, Guard) :-
            builtin(Statement, Other, Guard).
        guarded(Statement, Other, Guard) :-
            conflict(Statement, Other, Guard).

        %! unguarded(?Statement, ?Other)
        %  Statement conflicts with Other, whatever holds.
        unguarded(Statement, Other) :- setting(metaConflicts), !,
            Statement = [A], Other = [B],
            graphStore::argumentFor([conflict(A, B)], _).
        unguarded(Statement, Other) :-
            conflict(Statement, Other).

        %! statement(?Statement, ?Other, -ConflictStatement)
        %  With metaConflicts, the arguments for ConflictStatement make Statement conflict with Other.
        statement([A], [B], conflict(A, B, _)) :- setting(metaConflicts).
        statement([A], [B], conflict(A, B)) :- setting(metaConflicts).

        % The conflicts between a literal and its negation, and between obligations (o/1) and permissions (p/1)
        builtin([Atom], [-Atom], (Atom \= -_)).
        builtin([-Atom], [Atom], true).
        builtin([o(Atom)], [o(-Atom)], (Atom \= -_)).                   % O(x) <-> O(-x)
        builtin([o(-Atom)], [o(Atom)], true).
        builtin([p(Atom)], [o(-Atom)], (Atom \= -_)).                   % P(x) <-> O(-x)
        builtin([o(-Atom)], [p(Atom)], true).
        builtin([o(Atom)], [p(-Atom)], (Atom \= -_)).                   % O(x) <-> P(-x)
        builtin([p(-Atom)], [o(Atom)], true).
        builtin([o(Atom)], [-p(Atom)], true).                           % O(x) <-> O(-x) (-P(x))
        builtin([-p(Atom)], [o(Atom)], true).
        builtin([sup(X, Y)], [sup(Y, X)], true).

        % A guard holds: a goal that succeeds, or the term true
        holds(Guard) :- callable(Guard), !, call(Guard).
        holds(Guard) :- Guard.
        """.trimIndent()
}
