package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The arguments attacking through a conflict argument (module `conflictArguments`), with metaConflicts: conflicts
 * are then conclusions of the theory, and an argument attacks through the conflicts its conclusion has with others
 * only while their arguments stand.
 *
 * For each argument for a conflict on the conclusion of an argument, a copy of that argument is made, with
 * `mc(ConflictId, Conflict)` first among its rules: supported by the conflict argument (and by the supports of the
 * original), it is defeated with it, and it attacks through its conflict only (see the module attack). With
 * metaConflicts only the copies attack.
 */
object ConflictArguments : PrologModule("prolog.argumentation.graph.conflicts", "conflictArguments") {
    override val prologRawTheory: String =
        """
        %! buildAdditionalArguments
        %  With metaConflicts, the copies of the saved arguments for each argument for a conflict on their conclusion.
        buildAdditionalArguments :-
            findall(_, buildCopy, _).

        buildCopy :-
            setting(metaConflicts),
            conflictArgument(Conclusion, ConflictArgument),
            argument::claim(ConflictArgument, Conflict),
            context_id(argument(ConflictArgument), ConflictId),
            graphStore::argumentFor(Conclusion, Argument),
            \+ conflictOf(Argument, _),
            argument::rules(Argument, Rules),
            argument::withRules(Argument, [mc(ConflictId, Conflict)|Rules], Copy),
            graphStore::saveArgument(a, Conclusion, Copy),
            graphStore::saveSupport(ConflictArgument, Copy),
            cloneSupports(Argument, Copy),
            fail.
        buildCopy.

        % An argument for a conflict of the statement Conclusion with another one
        conflictArgument([A], ConflictArgument) :-
            graphStore::argumentFor([conflict(A, _, _)], ConflictArgument).
        conflictArgument([A], ConflictArgument) :-
            graphStore::argumentFor([conflict(A, _)], ConflictArgument).

        % The supports of Original also support Copy
        cloneSupports(Original, Copy) :-
            findall(_, (
                context_check(support(Support, Original)),
                graphStore::saveSupport(Support, Copy)
            ), _).

        %! conflictOf(+Argument, -Conflict)
        %  Argument is a copy made for an argument for Conflict.
        conflictOf(Argument, Conflict) :-
            argument::rules(Argument, [mc(_, Conflict)|_]).

        %! canAttack(+Argument)
        %  Argument may attack others: with metaConflicts only the copies do.
        canAttack(Argument) :- setting(metaConflicts), !,
            conflictOf(Argument, _).
        canAttack(_).
        """.trimIndent()
}
