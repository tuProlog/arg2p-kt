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
 * builder (graphBuilder, which graphBuildMode sets), the steps changing the graph (constraints with that graphExtension
 * or temporal, the copies of metaConflicts, the other graph extensions; see modifyGraph/0), the semantics
 * (argumentLabellingMode) and the statement labelling (statementLabellingMode). Query mode builds only a part of the
 * graph (see the module structuredPartial), then changes and labels it through this module as well.
 */
object StructuredWholeModule : PrologModule("prolog.argumentation.structured.whole", "structuredWhole") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuildMode, GraphBuilder)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! computeGlobalAcceptance
        %  The graph of the theory, changed by the steps the flags select, and its argument and statement labellings, in
        %  the context.
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
        %  The graph changed by the steps the flags select (see active/1), in the order of steps/1 whatever the order the
        %  flags state them in: first those adding the arguments derived from the others, each with their attacks (the
        %  instances of constraints, also with temporal, then the copies of metaConflicts), then those changing the
        %  attacks (rebutRestriction, the preference mode, bp). Any other graph extension (a module of the user) comes
        %  last, in the order stated.
        modifyGraph :-
            steps(Steps),
            applySteps(Steps),
            others(Others),
            modifyGraph(Others).

        %! querySteps(-Steps)
        %  The steps changing the graph (see modifyGraph/0) that relate statements in query mode, declaring
        %  queryRelations/0 (see queryExploration::extend/1), in their order.
        querySteps(Steps) :-
            steps(All),
            querySteps(All, Known),
            others(Others),
            querySteps(Others, Theirs),
            append(Known, Theirs, Steps).

        querySteps([], []).
        querySteps([Step|Steps], Query) :-
            (active(Step), Step::queryRelations -> Query = [Step|Rest] ; Query = Rest),
            querySteps(Steps, Rest).

        %! steps(-Steps)
        %  The modules changing the graph, in the order they apply.
        steps([constraints, conflictArguments, rebutRestriction, standardPref, defeasiblePref, defeasibleAllPref, abaPref, bp]).

        %! step(?Step)
        %  Step is one of steps/1.
        step(Step) :- steps(Steps), member(Step, Steps).

        %! active(+Step)
        %  Step applies: constraints with the instances of the theory (see parser::instances/0), conflictArguments when
        %  its conflicts are arguments (see conflict::argued/0), the others when they are graph extensions; any other
        %  graph extension is (see others/1).
        active(constraints) :- !, parser::instances.
        active(conflictArguments) :- !, conflict::argued.
        active(Step) :- setting(graphExtension, Step).

        % The graph extensions of the user, the modules other than those of steps/1, as stated
        others(Others) :-
            (setting(graphExtension, X), \+ step(X) ->
                findall(Y, (setting(graphExtension, Y), \+ step(Y)), Others)
            ;
                Others = []
            ).

        applySteps([]).
        applySteps([Step|Steps]) :-
            (active(Step) -> Step:::modifyArgumentationGraph ; true),
            applySteps(Steps).

        modifyGraph([]).
        modifyGraph([X|Ext]) :-
            X:::modifyArgumentationGraph,
            modifyGraph(Ext).

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
