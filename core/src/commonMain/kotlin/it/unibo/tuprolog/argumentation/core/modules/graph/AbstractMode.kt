package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The argumentation graph of a theory and its labellings, as the flags select them (module `abstract`): the graph
 * builder (graphBuildMode), the graph extensions (graphExtension), the semantics (argumentLabellingMode) and the
 * statement labelling (statementLabellingMode). It also labels abstract frameworks (solve/2,5).
 */
object AbstractMode : PrologModule("prolog.argumentation.abstract", "abstract") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuildMode)

    override val prologRawTheory: String =
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

        buildGraph :-
            setting(graphBuildMode, X),
            X:::buildArgumentationGraph.

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

        %! solve(+Arguments, +Attacks, -In, -Out, -Und)
        %  The labelling of the abstract framework of Arguments (atoms) and Attacks ((A, B) pairs), by argumentLabellingMode: the arguments IN, OUT and UND.
        solve(Arguments, Attacks, I, O, U) :-
            solve(Arguments, Attacks),
            graphStore::claimLabelling(I, O, U).

        %! solve(+Arguments, +Attacks)
        %  The lists are visited natively (for_each): as the goal of a findall, every step of the saving would visit them.
        %  The goals of for_each are solved in the module utils: they name the modules of their predicates.
        solve(Arguments, Attacks) :-
            context_reset,
            utils::for_each(A, Arguments, (
                abstract::abstractArgument(A, Argument),
                graphStore::saveArgument(graph, [A], Argument)
            )),
            utils::for_each((X, Y), Attacks, (
                abstract::abstractArgument(X, XX),
                abstract::abstractArgument(Y, YY),
                graphStore::saveAttack(abstract, XX, YY, YY)
            )),
            buildArgumentLabelling.

        % The argument for the abstract argument A: the strict rule A alone, concluding A
        abstractArgument(A, Argument) :-
            argument::infoParts(Info, [], [], []),
            argument::new([A], A, [A], [], Info, Argument).
        """.trimIndent()
}
