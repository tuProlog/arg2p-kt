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
        %  The theory is one of its profile (graphBuildMode): with aspic_defeasible, it has no strict rule nor axiom.
        checkProfile :-
            setting(graphBuildMode, aspic_defeasible),
            parser::checkStrict(_), !,
            throw(error(domain_error(defeasible_theory, strict), 'aspic_defeasible: the theory has strict rules or axioms')).
        checkProfile.

        %! modifyGraph
        %  The graph changed by each graph extension, the last stated first.
        modifyGraph :-
            findall(X, setting(graphExtension, X), Ext),
            modifyGraph(Ext).

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
