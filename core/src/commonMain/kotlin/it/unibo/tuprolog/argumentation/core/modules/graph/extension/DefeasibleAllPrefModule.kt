package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The preferences between rules as conclusions of the theory, taken together (module `defeasibleAllPref`, a
 * graphExtension): as defeasiblePref, but the attack argument is attacked by an argument for all the preferences
 * invalidating it, `preference(Preferences)`, supported by the arguments for each of them.
 */
object DefeasibleAllPrefModule : PrologModule(
    "prolog.argumentation.graph.preferences.defeasible.generic",
    "defeasibleAllPref",
) {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  The attacks the preferences may invalidate, as arguments, attacked by the arguments for the preferences.
        modifyArgumentationGraph :-
            defeasiblePref::assertAllSup,
            graphStore::attacks(Attacks),
            defeasiblePref::filterSupRelatedAttacks(Attacks, InvalidAttacks),
            defeasiblePref::convertAttacks(InvalidAttacks),
            buildPrefAttacks.

        buildPrefAttacks :-
            defeasiblePref::attackArgument(Attack, T, A, B, C),
            defeasiblePref::invalid(T, A, B, C, Preferences),
            createSuperiorityArgument(Preferences, Argument),
            graphStore::saveAttack(pref, Argument, Attack, Attack),
            defeasiblePref::prefersTransitively(Argument, Attack),
            fail.
        buildPrefAttacks.

        % The argument for preference(Preferences), supported by the arguments for each of the preferences, and
        % attacked as they are
        createSuperiorityArgument(Preferences, Argument) :-
            findall(Support, (
                member(X, Preferences),
                argument::claim(Support, X),
                graphStore::argument(Support)
            ), Supports),
            mergeIds(Supports, MergedId),
            argument::infoParts(Info, [pref], MergedId, []),
            argument::new(MergedId, pref, [preference(Preferences)], [], Info, Argument),
            graphStore::saveArgument(a, [preference(Preferences)], Argument),
            findall(_, (
                member(A, Supports),
                graphStore::saveSupport(A, Argument),
                findall(_, (
                    graphStore::attack(X, Y, A, Z),
                    graphStore::saveAttack(X, Y, Argument, Z)
                ), _)
            ), _).

        % The rules of the arguments, after pref
        mergeIds([], [pref]).
        mergeIds(Arguments, [pref|MergedId]) :-
            findall(Rules, (member(A, Arguments), argument::rules(A, Rules)), Res),
            utils::appendLists(Res, MergedId).

        %! queryDependencies(+Argument, -Statement)
        %  Query mode: the same arguments matter as for defeasiblePref
        queryDependencies(Argument, S) :- defeasiblePref::queryDependencies(Argument, S).
        %! queryDependants(+Argument, -Statement)
        queryDependants(Argument, S) :- defeasiblePref::queryDependants(Argument, S).
        """.trimIndent()
}
