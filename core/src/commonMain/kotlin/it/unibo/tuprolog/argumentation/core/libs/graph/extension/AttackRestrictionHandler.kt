package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The restricted rebut (module `rebutRestriction`, a graphExtension): a rebut only succeeds on an argument whose top
 * rule is defeasible.
 */
object AttackRestrictionHandler : PrologModule("prolog.argumentation.graph.rebutrestriction", "rebutRestriction") {
    override val prologRawTheory: String =
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

        % The attack types the restriction does not apply to. Everything else is restricted, including the attack
        % types introduced by other extensions: an unknown attack type is subject to the restriction rather than
        % dropped, so that the result does not depend on the order in which the extensions run.
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
