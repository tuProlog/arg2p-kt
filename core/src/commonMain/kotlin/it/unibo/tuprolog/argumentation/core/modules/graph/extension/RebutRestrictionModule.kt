package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The restricted rebut (module `rebutRestriction`, a graphExtension): a rebut only succeeds on an argument whose top
 * rule is defeasible.
 */
object RebutRestrictionModule : PrologModule("prolog.argumentation.graph.rebutrestriction", "rebutRestriction") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  The restricted attacks on an argument whose top rule is strict are removed.
        modifyArgumentationGraph :-
            findall(_, (
                graphStore::attack(T, A, B, C),
                checkRebutRestriction(T, A, B, C)
            ), _).

        checkRebutRestriction(T, A, B, C) :-
            \+ rebutRestriction(T, A, B, C),
            graphStore::removeAttack(T, A, B, C).

        % The attack types the restriction does not apply to. Everything else is restricted: rebuts, and the attacks
        % of a graph extension of the user.
        rebutRestriction(undermine, _, _, _).
        rebutRestriction(contrary_rebut, _, _, _).
        rebutRestriction(contrary_undermine, _, _, _).
        rebutRestriction(undercut, _, _, _).
        rebutRestriction(_, _, _, C) :- restrict(C).

        % The top rule of the argument is its last defeasible rule
        restrict(Argument) :-
            argument::topRule(Argument, TopRule),
            argument::lastDefeasibleRules(Argument, [TopRule]),
            TopRule \== none.
        """.trimIndent()
}
