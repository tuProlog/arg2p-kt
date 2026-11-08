package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The evaluation of the whole argumentation graph of a theory (module `structuredWhole`): the graph built by the builder of its
 * profile, then changed and labelled (see the module pipeline).
 */
object StructuredWholeModule : PrologModule("prolog.argumentation.structured.whole", "structuredWhole") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! computeGlobalAcceptance
        %  The graph of the theory, changed by the steps the flags select, and its argument and statement labellings, in
        %  the context.
        computeGlobalAcceptance :-
            buildGraph,
            pipeline::modifyGraph,
            pipeline::buildArgumentLabelling,
            pipeline::buildStatementLabelling.

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
        %  The graph of the theory, by the builder of its profile (graphBuilder), once the theory is checked against it:
        %  with the solvers of a session building it together, the graph of all of them (see graphStore::graph_begin/1,
        %  graphStore::graph_settle/0).
        buildGraph :-
            pipeline::checkProfile,
            graphStore::graph_begin(whole),
            pipeline::builder(X),
            pipeline::dispatched(X, buildArgumentationGraph/0),
            X:::buildArgumentationGraph,
            graphStore::graph_settle.
        """.trimIndent()
}
