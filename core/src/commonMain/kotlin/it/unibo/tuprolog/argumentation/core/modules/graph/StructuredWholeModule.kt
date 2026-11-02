package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The whole argumentation graph of a theory and its labellings, as the flags select them (module `structuredWhole`): the graph
 * builder (graphBuildMode), the graph extensions (graphExtension), the semantics (argumentLabellingMode) and the
 * statement labelling (statementLabellingMode). Query mode builds only a part of the graph (see the module structuredPartial),
 * then changes and labels it through this module as well.
 */
object StructuredWholeModule : PrologModule("prolog.argumentation.structured.whole", "structuredWhole") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuildMode, GraphBuilder)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! computeGlobalAcceptance
        %  The graph of the theory, changed by the extensions, and its argument and statement labellings, in the context.
        computeGlobalAcceptance :-
            buildGraph,
            modifyGraph,
            buildArgumentLabelling,
            buildStatementLabelling.

        %! computeGlobalAcceptance(-Graph, -ArgumentLabels, -StatementLabels)
        %  As computeGlobalAcceptance/0, with the graph ([Arguments, Attacks, Supports]) and the labels ([In, Out, Und]) collected.
        computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]) :-
            computeGlobalAcceptance,
            graphStore::graph(Arguments, Attacks, Supports),
            graphStore::argumentLabelling(ArgsIn, ArgsOut, ArgsUnd),
            graphStore::statementLabelling(StatIn, StatOut, StatUnd).

        %! buildGraph
        %  The graph of the theory, by the builder of its profile (graphBuilder), once the theory is checked against it.
        buildGraph :-
            checkProfile,
            builder(X),
            X:::buildArgumentationGraph.

        %! builder(-Builder)
        %  The module building the graph: that of the flag graphBuilder, ruleGraph if no setting names one.
        builder(X) :- setting(graphBuilder, X), !.
        builder(ruleGraph).

        %! checkProfile
        %  The theory is written in the languages of its profile (see parser::checkLanguages), and its graph extensions
        %  go together: one preference mode at most (see preferenceMode/1), abaPref only for ABA+ (the profile
        %  aba_plus).
        checkProfile :-
            parser::checkLanguages,
            checkExtensions.

        checkExtensions :-
            findall(P, (setting(graphExtension, P), preferenceMode(P)), Modes),
            (Modes = [_, _|_] ->
                throw(error(domain_error(one_preference_mode, Modes), 'graphExtension: one preference mode at most (standardPref, defeasiblePref, defeasibleAllPref or abaPref)'))
            ;
                true
            ),
            (setting(graphExtension, abaPref), \+ setting(graphBuildMode, aba_plus) ->
                throw(error(domain_error(aba_plus_theory, abaPref), 'graphExtension: abaPref is the preference mode of ABA+ alone (graphBuildMode(aba_plus))'))
            ;
                true
            ).

        %! preferenceMode(?Extension)
        %  Extension weighs the attacks by the preferences: a theory has one at most.
        preferenceMode(standardPref).
        preferenceMode(defeasiblePref).
        preferenceMode(defeasibleAllPref).
        preferenceMode(abaPref).

        %! modifyGraph
        %  The graph changed by the steps the flags select (see active/1), in the order of step/1 whatever the order the
        %  flags state them in: first those adding the arguments derived from the others, each with their attacks (the
        %  instances of constraints, also with temporal, then the copies of metaConflicts), then those changing the
        %  attacks (rebutRestriction, the preference mode, bp). Any other graph extension (a module of the user) comes
        %  last, the last stated first.
        modifyGraph :-
            findall(S, (step(S), active(S)), Steps),
            findall(X, (setting(graphExtension, X), \+ step(X)), Others),
            applySteps(Steps),
            modifyGraph(Others).

        %! step(?Step)
        %  The modules changing the graph, in the order they apply.
        step(constraints).
        step(conflictArguments).
        step(rebutRestriction).
        step(standardPref).
        step(defeasiblePref).
        step(defeasibleAllPref).
        step(abaPref).
        step(bp).

        %! active(+Step)
        %  Step applies: constraints with that graph extension or temporal, conflictArguments with metaConflicts, the
        %  others when they are graph extensions. The settings are read here, as a call to another module costs as much
        %  as the graph it is made over.
        active(constraints) :- !, (setting(graphExtension, constraints) -> true ; setting(temporal)).
        active(conflictArguments) :- !, setting(metaConflicts).
        active(Step) :- setting(graphExtension, Step).

        applySteps([]).
        applySteps([Step|Steps]) :-
            Step:::modifyArgumentationGraph,
            applySteps(Steps).

        modifyGraph([]).
        modifyGraph([X|Ext]) :-
            modifyGraph(Ext),
            X:::modifyArgumentationGraph.

        %! buildArgumentLabelling
        %  The labelling of the arguments, by the semantics of argumentLabellingMode.
        buildArgumentLabelling :-
            setting(argumentLabellingMode, X),
            X:::argumentLabelling.

        %! buildStatementLabelling
        %  The labelling of the statements, by statementLabellingMode.
        buildStatementLabelling :-
            setting(statementLabellingMode, X),
            X:::statementLabelling.
        """.trimIndent()
}
