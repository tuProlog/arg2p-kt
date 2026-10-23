package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The preferences between the rules of the theory, applied to the attacks (module `standardPref`, a graphExtension):
 * an attack fails when the attacked argument is superior to the attacker (see the module superiority).
 */
object StandardPrefModule : PrologModule("prolog.argumentation.graph.preferences.strict", "standardPref") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  The attacks failing for the preferences are removed. Attacks are visited by argument identifiers, and
        %  arguments are compared by their info only: the whole arguments are only retrieved to remove an attack.
        modifyArgumentationGraph :-
            findall(_, (
                graphStore::attackById(T, IdA, IdB, IdC),
                checkStandardPreference(T, IdA, IdB, IdC)
            ), _).

        % Negation is written as cut and fail: \+ would go through call/1, which visits the whole goal before running it
        checkStandardPreference(T, IdA, _, IdC) :- standardPreferences(T, IdA, IdC), !, fail.
        checkStandardPreference(T, IdA, IdB, IdC) :-
            graphStore::argumentById(IdA, A),
            graphStore::argumentById(IdB, B),
            graphStore::argumentById(IdC, C),
            graphStore::removeAttack(T, A, B, C).

        % The attack types preferences do not apply to. Everything else is weighed against the preference order,
        % including the attack types introduced by other extensions: an unknown attack type is compared rather than
        % dropped, so that the result does not depend on the order in which the extensions run.
        standardPreferences(contrary_rebut, _, _) :- !.
        standardPreferences(contrary_undermine, _, _) :- !.
        standardPreferences(undercut, _, _) :- !.
        standardPreferences(_, IdA, IdC) :-
            graphStore::argumentInfo(IdA, InfoIdA, InfoA),
            graphStore::argumentInfo(IdC, InfoIdC, InfoC),
            superiorInfo(InfoIdC, InfoC, InfoIdA, InfoA), !,
            fail.
        standardPreferences(_, _, _).

        % Superiority only depends on the info of the arguments, shared by many of them: it is buffered by their
        % identifiers, so that each pair is compared once
        superiorInfo(InfoIdC, _, InfoIdA, _) :-
            context_check(infoSuperior(InfoIdC, InfoIdA, Result)), !,
            Result == yes.
        superiorInfo(InfoIdC, InfoC, InfoIdA, InfoA) :-
            superiority::superiorInfo(InfoC, InfoA, _), !,
            context_assert(infoSuperior(InfoIdC, InfoIdA, yes)).
        superiorInfo(InfoIdC, _, InfoIdA, _) :-
            context_assert(infoSuperior(InfoIdC, InfoIdA, no)),
            fail.
        """.trimIndent()
}
