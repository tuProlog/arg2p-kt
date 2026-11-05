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

        %! computeStatementLabels(-In, -Out, -Und)
        %  As computeGlobalAcceptance/0, with the labels of the statements, one solution per labelling.
        computeStatementLabels(StatIn, StatOut, StatUnd) :-
            computeGlobalAcceptance,
            graphStore::statementLabelling(StatIn, StatOut, StatUnd).

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
            dispatched(X, buildArgumentationGraph/0),
            X:::buildArgumentationGraph.

        %! dispatched(+Module, +Predicate)
        %  Module defines Predicate (Name/Arity), one the pipeline calls on the modules the flags select: an existence
        %  error otherwise, rather than the failure of the whole evaluation an unknown predicate gives.
        dispatched(Module, Name/Arity) :-
            (module_defines(Module, Name/Arity) ->
                true
            ;
                throw(error(existence_error(procedure, ':'(Module, Name/Arity)), 'the module does not define the predicate the pipeline calls on it'))
            ).

        %! builder(-Builder)
        %  The module building the graph: that of the flag graphBuilder, ruleGraph if no setting names one.
        builder(X) :- setting(graphBuilder, X), !.
        builder(ruleGraph).

        %! checkProfile
        %  The theory is written in the languages of its profile (see parser::checkLanguages), and its flags go
        %  together: one preference mode at most (see preferenceMode/1); abaPref only for ABA+ (the profile aba_plus),
        %  and an ABA theory (the language abaLanguage) with no other preference mode, nor the meta-argumentation of
        %  ASPIC+ (metaConflicts, metaRules).
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
            (setting(graphExtension, abaPref), \+ setting(graphLanguage, abaLanguage) ->
                throw(error(domain_error(aba_theory, abaPref), 'graphExtension: abaPref is the preference mode of ABA theories alone (graphLanguage(abaLanguage), as the profiles aba and aba_plus state)'))
            ;
                true
            ),
            (setting(graphLanguage, abaLanguage), member(M, Modes), M \== abaPref ->
                throw(error(domain_error(aba_theory, M), 'graphExtension: an ABA theory has no preferences between rules (standardPref, defeasiblePref, defeasibleAllPref): abaPref for ABA+'))
            ;
                true
            ),
            (setting(graphLanguage, abaLanguage), (setting(metaConflicts) -> Meta = metaConflicts ; setting(metaRules) -> Meta = metaRules) ->
                throw(error(domain_error(aba_theory, Meta), 'aba: the meta-argumentation of ASPIC+ (metaConflicts, metaRules) does not apply to an ABA theory'))
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
        %  The steps changing the graph (see modifyGraph/0) that relate statements in query mode, defining
        %  queryDependencies/2 and queryDependants/2 (see queryExploration::extend/1), in their order.
        querySteps(Steps) :-
            steps(All),
            querySteps(All, Known),
            others(Others),
            querySteps(Others, Theirs),
            append(Known, Theirs, Steps).

        querySteps([], []).
        querySteps([Step|Steps], Query) :-
            (active(Step), module_defines(Step, queryDependencies/2), module_defines(Step, queryDependants/2) ->
                Query = [Step|Rest]
            ;
                Query = Rest
            ),
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
        others(Others) :- findall(Y, (setting(graphExtension, Y), \+ step(Y)), Others).

        applySteps([]).
        applySteps([Step|Steps]) :-
            (active(Step) -> Step:::modifyArgumentationGraph ; true),
            applySteps(Steps).

        modifyGraph([]).
        modifyGraph([X|Ext]) :-
            dispatched(X, modifyArgumentationGraph/0),
            X:::modifyArgumentationGraph,
            modifyGraph(Ext).

        %! buildArgumentLabelling
        %  The labelling of the arguments, by the semantics of argumentLabellingMode.
        buildArgumentLabelling :-
            setting(argumentLabellingMode, X),
            dispatched(X, argumentLabelling/0),
            X:::argumentLabelling.

        %! buildStatementLabelling
        %  The labelling of the statements, by statementLabellingMode.
        buildStatementLabelling :-
            setting(statementLabellingMode, X),
            dispatched(X, statementLabelling/0),
            X:::statementLabelling.
        """.trimIndent()
}
