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
        %  As defeasiblePref, the attack arguments attacked by an argument for all the preferences invalidating them
        %  together (see defeasiblePref::modifyArgumentationGraph/1).
        modifyArgumentationGraph :-
            defeasiblePref::modifyArgumentationGraph(all).

        %! queryDependencies(+Argument, -Statement)
        %  Query mode: the same arguments matter as for defeasiblePref
        queryDependencies(Argument, S) :- defeasiblePref::queryDependencies(Argument, S).
        %! queryDependants(+Argument, -Statement)
        queryDependants(Argument, S) :- defeasiblePref::queryDependants(Argument, S).
        """.trimIndent()
}
