package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The preferences between rules as conclusions of the theory, taken together (module `defeasibleAllPref`, a
 * graphExtension): as defeasiblePref, but the attack argument is attacked by an argument for all the preferences
 * invalidating it, `preference(Preferences)`, supported by one argument for each of them: one such argument for each
 * choice of those arguments, each standing or falling with its own.
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
            argumentsFor(Preferences, Supports),
            createSuperiorityArgument(Preferences, Supports, Argument),
            graphStore::saveAttack(pref, Argument, Attack, Attack),
            fail.
        buildPrefAttacks.

        % Supports is each choice of one argument for each of Preferences, in their order
        argumentsFor([], []).
        argumentsFor([X|Xs], [Support|Supports]) :-
            argument::claim(Support, X),
            graphStore::argument(Support),
            argumentsFor(Xs, Supports).

        % The argument for preference(Preferences) supported by Supports, one argument for each of them, and so attacked
        % as they are (see the module graphStore): saved once, for the first attack the preferences invalidate
        createSuperiorityArgument(Preferences, Supports, Argument) :-
            mergeIds(Supports, MergedId),
            argument::infoParts(Info, [pref], MergedId, []),
            argument::new(MergedId, pref, [preference(Preferences)], [], Info, Argument),
            (graphStore::saveArgument(a, [preference(Preferences)], Argument) -> saveRelations(Supports, Argument) ; true).

        saveRelations(Supports, Argument) :-
            findall(_, (
                member(A, Supports),
                graphStore::saveSupport(A, Argument)
            ), _).

        % The rules of the arguments, after pref
        mergeIds([], [pref]).
        mergeIds(Arguments, [pref|MergedId]) :-
            findall(Rules, (member(A, Arguments), argument::rules(A, Rules)), Res),
            utils::appendLists(Res, MergedId).

        %! queryRelations
        %  The module relates statements in query mode, as defeasiblePref does: see queryExploration::extend/1.
        queryRelations.

        %! queryDependencies(+Argument, -Statement)
        %  Query mode: the same arguments matter as for defeasiblePref
        queryDependencies(Argument, S) :- defeasiblePref::queryDependencies(Argument, S).
        %! queryDependants(+Argument, -Statement)
        queryDependants(Argument, S) :- defeasiblePref::queryDependants(Argument, S).
        """.trimIndent()
}
