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
        %  arguments are compared by their info only.
        modifyArgumentationGraph :-
            findall(a(T, IdA, IdC), (
                graphStore::attackById(T, IdA, IdC, IdC),
                failing(T, IdA, IdC)
            ), Failing),
            graphStore::removeAttackIds(Failing).

        failing(T, IdA, IdC) :- standardPreferences(T, IdA, IdC), !, fail.
        failing(_, _, _).

        % The attack types preferences do not apply to. Everything else is weighed against the preference order: rebuts
        % and undermines, and the attacks of a graph extension of the user.
        standardPreferences(contrary_rebut, _, _) :- !.
        standardPreferences(contrary_undermine, _, _) :- !.
        standardPreferences(undercut, _, _) :- !.
        standardPreferences(_, IdA, IdC) :-
            graphStore::argumentInfoId(IdA, InfoIdA),
            graphStore::argumentInfoId(IdC, InfoIdC),
            superiorById(InfoIdC, InfoIdA), !,
            fail.
        standardPreferences(_, _, _).

        % Superiority only depends on the info of the arguments, shared by many of them: it is buffered by their
        % identifiers, so that each pair is read and compared once
        superiorById(InfoIdC, InfoIdA) :-
            context_check(infoSuperior(InfoIdC, InfoIdA, Result)), !,
            Result == yes.
        superiorById(InfoIdC, InfoIdA) :-
            graphStore::infoById(InfoIdC, InfoC),
            graphStore::infoById(InfoIdA, InfoA),
            superiority::compareInfo(InfoC, InfoA, _), !,
            context_assert(infoSuperior(InfoIdC, InfoIdA, yes)).
        superiorById(InfoIdC, InfoIdA) :-
            context_assert(infoSuperior(InfoIdC, InfoIdA, no)),
            fail.
        """.trimIndent()
}
