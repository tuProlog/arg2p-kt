package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * What every evaluation goes through, whatever part of the graph it builds (module `pipeline`), as the flags select it: the
 * theory checked against its profile, the graph builder (graphBuilder, which graphBuildMode sets), the steps changing the
 * graph (the conflicts as arguments, the graph extensions; see modifyGraph/0), the semantics (argumentLabellingMode) and the
 * statement labelling (statementLabellingMode). The modules structuredWhole and structuredPartial build the graph, the module
 * abstract states it.
 */
object PipelineModule : PrologModule("prolog.argumentation.pipeline", "pipeline") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuildMode, GraphBuilder)

    override val prologRawTheory: String =
        // language=Prolog
        """
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
        %  conflicts as arguments of metaConflicts), then those changing the
        %  attacks (rebutRestriction, the preference mode, bp). Any other graph extension (a module of the user) comes
        %  last, in the order stated.
        %  With the solvers of a session changing it together (see GraphSharing), each step once every solver has merged
        %  what the others did before it, those changing the attacks on each argument alone (see stepScope/2) going
        %  through the attacks on the arguments of each solver.
        modifyGraph :-
            steps(Steps),
            applySteps(Steps),
            others(Others),
            modifyGraph(Others),
            graphStore::graph_step(all).

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
        %  Step applies: conflictArguments when its conflicts are arguments (see conflict::argued/0), the others when they
        %  are graph extensions; any other
        %  graph extension is (see others/1).
        active(constraints) :- !, parser::instances.
        active(conflictArguments) :- !, conflict::argued.
        active(Step) :- setting(graphExtension, Step).

        % The graph extensions of the user, the modules other than those of steps/1, as stated
        others(Others) :- findall(Y, (setting(graphExtension, Y), \+ step(Y)), Others).

        applySteps([]).
        applySteps([Step|Steps]) :-
            stepScope(Step, Scope),
            graphStore::graph_step(Scope),
            (active(Step) -> Step:::modifyArgumentationGraph ; true),
            applySteps(Steps).

        % stepScope(+Step, -Scope): own for a step going through the attacks (attackById/4, directAttacks/1 of the module
        % graphStore), or the arguments it changes (its enumerations of them), each alone, which the solvers of a session building
        % the graph together divide by the arguments they saved, all of them looked up; all for the others, each solver
        % changing the whole graph alike (bp labels it in the chain of its burdens)
        stepScope(constraints, own) :- !.
        stepScope(conflictArguments, own) :- !.
        stepScope(rebutRestriction, own) :- !.
        stepScope(standardPref, own) :- !.
        stepScope(defeasiblePref, own) :- !.
        stepScope(defeasibleAllPref, own) :- !.
        stepScope(abaPref, own) :- !.
        stepScope(_, all).

        modifyGraph([]).
        modifyGraph([X|Ext]) :-
            graphStore::graph_step(all),
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
