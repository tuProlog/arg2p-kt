package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The evaluation of the part of the argumentation graph the labels of the statements for a goal depend on (module
 * `structuredPartial`), as the locality of the semantics goes (see locality/1): the part explored from the goal (see the module
 * queryExploration), then changed and labelled as the whole graph is (see the module pipeline). The solvers of a session
 * building it together (see GraphSharing) explore it each with its rules, through the graph store.
 */
object StructuredPartialModule : PrologModule("prolog.argumentation.structured.partial", "structuredPartial") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! locality(-Locality)
        %  The part of the graph the labels of the arguments for the query depend on, as the semantics declares it
        %  (locality/1): the arguments they depend on (directional), those connected to them in either direction
        %  (connected), or the whole graph (whole_graph, also for a semantics that does not declare it). Only rules are
        %  explored: a graph built otherwise (a graphBuilder other than ruleGraph, as for an abstract theory) is whole.
        locality(Locality) :-
            pipeline::builder(X),
            (X == ruleGraph -> semanticsLocality(Locality) ; Locality = whole_graph).

        semanticsLocality(Locality) :-
            setting(argumentLabellingMode, Semantics),
            (module_defines(Semantics, locality/1) -> Semantics::locality(Locality) ; Locality = whole_graph).

        %! computeGoalAcceptance(+Goal, +Locality)
        %  The labels of the statements for Goal, in the context, on the part of the graph they depend on as Locality goes
        %  (see queryGraph/2), changed and labelled as the whole graph is.
        computeGoalAcceptance(Goal, Locality) :-
            queryGraph(Goal, Locality),
            pipeline:::modifyGraph,
            pipeline:::buildArgumentLabelling,
            pipeline:::buildStatementLabelling.

        %! queryGraph(+Query, +Locality)
        %  The part of the graph the labels of the arguments for Query depend on, as Locality goes: its arguments and the
        %  attacks between them, not changed by the steps of the graph yet. With the solvers of a session exploring it
        %  together (see GraphSharing), what the others explore and build carried on with, and the graph of all of them
        %  merged once their attacks are found.
        queryGraph(Query, Locality) :-
            pipeline::checkProfile,
            graphStore::graph_begin(Query),
            queryExploration::explore(Query, Locality),
            sharedExploration(Locality),
            ruleGraph:::buildAttacks,
            graphStore::graph_settle.

        % sharedExploration(+Locality): the exploration carried on with what the other solvers of a session explore and
        % build, until none has more (see graphStore::graph_explore/2); nothing for a solver alone. The statements they
        % explored are explored here too, with the rules here, and those whose rules use the conclusions of the arguments
        % they built again (see queryExploration::resume/1): what an argument makes worth exploring is explored by its
        % builder (see queryExploration::dependency/3)
        sharedExploration(Locality) :-
            graphStore::graph_explore(Statements, Claims), !,
            queryExploration::exploreAll(Statements),
            queryExploration::resume(Claims),
            queryExploration::expand(Locality),
            queryExploration::extend(Locality),
            sharedExploration(Locality).
        sharedExploration(_).
        """.trimIndent()
}
