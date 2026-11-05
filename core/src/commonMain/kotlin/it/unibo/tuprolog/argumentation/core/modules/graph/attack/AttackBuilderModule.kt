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
        %  over the graph go through its copy by identifiers. None when the conflicts are arguments (see
        %  conflict::argued/0): only their copies attack, added when the graph is changed (see the module
        %  conflictArguments).
        buildAttacks :-
            conflict::argued, !.
        buildAttacks :-
            (parser::constrained -> Constrained = yes ; Constrained = no),
            findall(_, buildDirectAttack(Constrained), _).

        % (Constrained: the theory may have constraints, which the two arguments must satisfy together with the
        % bindings of their conflict, see constraint::compatible/2)
        buildDirectAttack(Constrained) :-
            graphStore::argumentById(IdA, A),
            attackCandidates::attackersOf(IdA, A, Conflict),
            findAttacker(Conflict, B),
            directAttack(Constrained, T, B, A),
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

        % (from every argument to its attackers, not from those added to what they attack: the catalog of the parallel solver has
        % no rule, which that way goes through)
        buildAddedAttack(Constrained) :-
            graphStore::argumentById(IdA, A),
            attackCandidates::possibleAttackers(IdA, A, Conflict),
            findAttacker(Conflict, B),
            graphStore::argumentId(B, IdB),
            (context_check(addedArgument(IdA)) -> true ; context_check(addedArgument(IdB))),
            directAttack(Constrained, T, B, A),
            graphStore::saveAttackById(T, IdB, B, IdA, A, IdA, A),
            fail.
        buildAddedAttack(_).

        %! attacksOn(+Ids, -Attacks)
        %  The direct attacks on the saved arguments with identifiers Ids, by any saved argument, not saved: each
        %  a(Type, IdAttacker, IdAttacked). What the workers of the parallel solver find.
        attacksOn(Ids, Attacks) :-
            (parser::constrained -> Constrained = yes ; Constrained = no),
            findall(a(T, IdB, IdA), attackOn(Constrained, Ids, T, IdB, IdA), Attacks).

        attackOn(Constrained, Ids, T, IdB, IdA) :-
            member(IdA, Ids),
            graphStore::argumentById(IdA, A),
            attackCandidates::possibleAttackers(IdA, A, Conflict),
            findAttacker(Conflict, B),
            directAttack(Constrained, T, B, A),
            graphStore::argumentId(B, IdB).

        % (Constrained: whether the theory may have constraints, found once for all the attacks of a graph.) Attacks are
        % checked on copies, so that the arguments are saved exactly as they are stored: binding their variables would
        % change their identifiers (see context_id)
        directAttack(Constrained, T, B, A) :-
            conflict::attacker(B),
            copy_term(B-A, CB-CA),
            attackRelation::attacks(T, CB, CA),
            (Constrained == yes -> constraint::compatible(CB, CA) ; true).

        % Finds the stored arguments whose conclusion unifies with the given one, without instantiating them
        findAttacker([Head], B) :- !,
            graphStore::argumentUnifiable([Head], B).
        findAttacker(Conclusion, B) :-
            graphStore::argumentFor(Conclusion, B).
        """.trimIndent()
}
