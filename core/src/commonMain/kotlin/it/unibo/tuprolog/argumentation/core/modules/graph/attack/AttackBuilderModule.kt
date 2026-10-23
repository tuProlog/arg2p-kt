package it.unibo.tuprolog.argumentation.core.modules.graph.attack

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The attacks between the arguments of the graph (module `attackBuilder`): each argument against those that may
 * attack it (see the module attackCandidates), as the module attackRelation states them, and the super-arguments of
 * the attacked one, saved in the context (see the module graphStore).
 */
object AttackBuilderModule : PrologModule("prolog.argumentation.graph.attacks", "attackBuilder") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildAttacks
        %  The attacks between the saved arguments, direct and on their super-arguments. Loops over the graph go
        %  through its copy by identifiers (see the module graphStore).
        buildAttacks :-
            findall(_, buildDirectAttack, _).

        buildDirectAttack :-
            graphStore::argumentById(IdA, A),
            attackCandidates::possibleAttackers(IdA, A, Conflict),
            findAttacker(Conflict, B),
            conflictArguments::canAttack(B),
            A \== B,
            % Attacks are checked on copies, so that A and B are saved exactly as they are stored:
            % binding their variables would change their identifiers (see context_id)
            copy_term(B-A, CB-CA),
            attackRelation::attacks(T, CB, CA),
            graphStore::argumentId(B, IdB),
            graphStore::saveAttackById(T, IdB, B, IdA, A, IdA, A),
            buildTransitiveAttacks(T, IdB, B, IdA, A),
            fail.
        buildDirectAttack.

        % Finds the stored arguments whose conclusion unifies with the given one, without instantiating them
        findAttacker([Head], B) :- !,
            graphStore::argumentUnifiable([Head], B).
        findAttacker(Conclusion, B) :-
            graphStore::argumentFor(Conclusion, B).

        %! buildTransitiveAttacks(+Type, +IdB, +B, +IdA, +A)
        %  B, attacking A, attacks every argument that A supports, directly or not.
        buildTransitiveAttacks(T, IdB, B, IdA, A) :-
            findall(_, (
                graphStore::supportedById(IdA, IdC),
                graphStore::argumentById(IdC, C),
                graphStore::saveAttackById(T, IdB, B, IdC, C, IdA, A)
            ),_).
        """.trimIndent()
}
