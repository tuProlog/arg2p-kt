package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.ConflictModel
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The conflicts between statements (module `conflict`). Without metaConflicts they are those of the parts of the
 * conflict model stated (conflictModel, all of them by default): negation (a literal and its negation), deontic (the
 * obligations and permissions in conflict), preference (a superiority and its converse) and declared (those the
 * theory declares, conflict/2,3); with metaConflicts they are only those concluded by an argument for
 * `conflict(A, B)` or `conflict(A, B, Guard)`.
 *
 * The conflicts the languages of the theory state (graphLanguage) are kept in the context by the parser, and hold
 * whatever the parts: with none of them (`conflictModel(none)`), they are the only ones.
 *
 * Statements are written as in the conclusions of arguments, `[Claim]`. A conflict may hold only under a guard, a
 * Prolog goal (or a term true or false) checked when the conflict is used.
 */
object ConflictModule : PrologModule("prolog.argumentation.language.conflict", "conflict") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(ConflictModel)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! between(?Statement, ?Other)
        %  Statement conflicts with Other, the guard of the conflict holding.
        between(Statement, Other) :-
            unguarded(Statement, Other).
        between(Statement, Other) :-
            guarded(Statement, Other, Guard),
            parser::holds(Guard).

        %! stated(+Conflict, ?Statement, ?Other)
        %  Conflict, a conflict(A, B) or conflict(A, B, Guard) term, makes Statement ([A]) conflict with Other ([B]),
        %  its guard holding.
        stated(conflict(A, B), [A], [B]).
        stated(conflict(A, B, Guard), [A], [B]) :-
            parser::holds(Guard).

        %! declared(?Statement, ?Other)
        %  The theory declares Statement in conflict with Other (conflict/2, or conflict/3 whatever its guard), and the
        %  conflict model counts the declared conflicts: never with metaConflicts, where the conflicts are the
        %  conclusions of arguments.
        declared(Statement, Other) :-
            \+ setting(metaConflicts),
            setting(conflictModel, declared),
            (user::conflict(Statement, Other) ; user::conflict(Statement, Other, _)).

        %! guarded(?Statement, ?Other, -Guard)
        %  Statement conflicts with Other when Guard holds.
        guarded(Statement, Other, Guard) :- setting(metaConflicts), !,
            Statement = [A], Other = [B],
            graphStore::argumentFor([conflict(A, B, Guard)], _).
        guarded(Statement, Other, Guard) :-
            setting(conflictModel, Part),
            builtin(Part, Statement, Other, Guard).
        guarded(Statement, Other, Guard) :-
            setting(conflictModel, declared),
            user::conflict(Statement, Other, Guard).

        %! unguarded(?Statement, ?Other)
        %  Statement conflicts with Other, whatever holds.
        unguarded(Statement, Other) :- setting(metaConflicts), !,
            Statement = [A], Other = [B],
            graphStore::argumentFor([conflict(A, B)], _).
        unguarded(Statement, Other) :-
            setting(conflictModel, declared),
            user::conflict(Statement, Other).
        unguarded(Statement, Other) :-
            context_check(conflict(Statement, Other)).

        %! argued
        %  The conflicts are the conclusions of arguments (metaConflicts): which statements conflict changes as the
        %  graph grows, and an argument attacks through the conflicts its conclusion has with others only while their
        %  arguments stand: through a copy of it carrying each (see the module conflictArguments).
        argued :- setting(metaConflicts).

        %! carried(+Argument, -Conflict)
        %  Argument carries Conflict: the copy of an argument made for an argument for Conflict, mc(Id, Conflict) first
        %  among its rules (see the module conflictArguments).
        carried(Argument, Conflict) :-
            argument::rules(Argument, [mc(_, Conflict)|_]).

        %! attacker(+Argument)
        %  Argument may attack others: any argument, or, with the conflicts argued, one carrying a conflict.
        attacker(Argument) :- argued, !, carried(Argument, _).
        attacker(_).

        %! statement(?Statement, ?Other, -ConflictStatement)
        %  With metaConflicts, the arguments for ConflictStatement make Statement conflict with Other.
        statement([A], [B], conflict(A, B, _)) :- setting(metaConflicts).
        statement([A], [B], conflict(A, B)) :- setting(metaConflicts).

        %! builtin(?Part, ?Statement, ?Other, -Guard)
        %  Statement conflicts with Other when Guard holds, a conflict of the part Part of the conflict model.
        % negation: a literal and its negation
        builtin(negation, [Atom], [-Atom], (Atom \= -_)).
        builtin(negation, [-Atom], [Atom], true).
        % deontic: obligations (o/1) and permissions (p/1)
        builtin(deontic, [o(Atom)], [o(-Atom)], (Atom \= -_)).          % O(x) <-> O(-x)
        builtin(deontic, [o(-Atom)], [o(Atom)], true).
        builtin(deontic, [p(Atom)], [o(-Atom)], (Atom \= -_)).          % P(x) <-> O(-x)
        builtin(deontic, [o(-Atom)], [p(Atom)], true).
        builtin(deontic, [o(Atom)], [p(-Atom)], (Atom \= -_)).          % O(x) <-> P(-x)
        builtin(deontic, [p(-Atom)], [o(Atom)], true).
        builtin(deontic, [o(Atom)], [-p(Atom)], true).                  % O(x) <-> O(-x) (-P(x))
        builtin(deontic, [-p(Atom)], [o(Atom)], true).
        % preference: a superiority and its converse
        builtin(preference, [sup(X, Y)], [sup(Y, X)], true).

        """.trimIndent()
}
