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
            findall(a(T, IdA, IdC), (
                graphStore::attackById(T, IdA, IdC, IdC),
                graphStore::argumentById(IdC, C),
                \+ rebutRestriction(T, C)
            ), Restricted),
            graphStore::removeAttackIds(Restricted).

        % The attack types the restriction does not apply to. Everything else is restricted: rebuts, and the attacks
        % of a graph extension of the user.
        rebutRestriction(undermine, _).
        rebutRestriction(contrary_rebut, _).
        rebutRestriction(contrary_undermine, _).
        rebutRestriction(undercut, _).
        % the top rule of the argument is its last defeasible rule (see attackRelation::defeasibleTopRule/2)
        rebutRestriction(_, C) :-
            attackRelation::defeasibleTopRule(C, _).
        """.trimIndent()
}
