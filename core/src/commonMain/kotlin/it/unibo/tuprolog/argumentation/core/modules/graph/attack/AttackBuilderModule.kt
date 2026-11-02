package it.unibo.tuprolog.argumentation.core.modules.graph.attack

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The attacks between the arguments of the graph (module `attackBuilder`): each argument against those that may
 * attack it (see the module attackCandidates), as the module attackRelation states them, saved in the context (see
 * the module graphStore, which reads them extended to the arguments built on the attacked one).
 */
object AttackBuilderModule : PrologModule("prolog.argumentation.graph.attacks", "attackBuilder") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildAttacks
        %  The direct attacks between the saved arguments, an argument attacking itself included (as r : a => undercut(r)
        %  does): those on the arguments built on the attacked one follow from them (see the module graphStore). Loops
        %  over the graph go through its copy by identifiers.
        buildAttacks :-
            (parser::constrained -> Constrained = yes ; Constrained = no),
            findall(_, buildDirectAttack(Constrained), _).

        % (Constrained: the theory may have constraints, which the two arguments must satisfy together with the
        % bindings of their conflict, see constraint::compatible/2)
        buildDirectAttack(Constrained) :-
            graphStore::argumentById(IdA, A),
            attackCandidates::possibleAttackers(IdA, A, Conflict),
            findAttacker(Conflict, B),
            conflictArguments::canAttack(B),
            % Attacks are checked on copies, so that A and B are saved exactly as they are stored:
            % binding their variables would change their identifiers (see context_id)
            copy_term(B-A, CB-CA),
            attackRelation::attacks(T, CB, CA),
            (Constrained == yes -> constraint::compatible(CB, CA) ; true),
            graphStore::argumentId(B, IdB),
            graphStore::saveAttackById(T, IdB, B, IdA, A, IdA, A),
            fail.
        buildDirectAttack(_).

        %! buildAttacksOf(+Ids)
        %  The direct attacks on and by the saved arguments with identifiers Ids, added to a graph with its attacks
        %  (those between the others are saved already).
        buildAttacksOf([]) :- !.
        buildAttacksOf(Ids) :-
            findall(_, (member(Id, Ids), context_assert(addedArgument(Id))), _),
            (parser::constrained -> Constrained = yes ; Constrained = no),
            findall(_, buildAddedAttack(Constrained), _),
            context_retract(addedArgument(_)).

        buildAddedAttack(Constrained) :-
            graphStore::argumentById(IdA, A),
            attackCandidates::possibleAttackers(IdA, A, Conflict),
            findAttacker(Conflict, B),
            graphStore::argumentId(B, IdB),
            (context_check(addedArgument(IdA)) -> true ; context_check(addedArgument(IdB))),
            conflictArguments::canAttack(B),
            copy_term(B-A, CB-CA),
            attackRelation::attacks(T, CB, CA),
            (Constrained == yes -> constraint::compatible(CB, CA) ; true),
            graphStore::saveAttackById(T, IdB, B, IdA, A, IdA, A),
            fail.
        buildAddedAttack(_).

        % Finds the stored arguments whose conclusion unifies with the given one, without instantiating them
        findAttacker([Head], B) :- !,
            graphStore::argumentUnifiable([Head], B).
        findAttacker(Conclusion, B) :-
            graphStore::argumentFor(Conclusion, B).
        """.trimIndent()
}
