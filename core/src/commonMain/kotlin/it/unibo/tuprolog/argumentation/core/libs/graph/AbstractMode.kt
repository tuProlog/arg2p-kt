package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object AbstractMode : PrologModule("prolog.argumentation.abstract", "abstract") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuildMode)

    override val prologRawTheory: String =
        """
        computeGlobalAcceptance :-
            buildGraph,
            modifyGraph,
            buildArgumentLabelling,
            buildStatementLabelling.

        computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]) :-
            computeGlobalAcceptance,
            utils::recoverGraph(Arguments, Attacks, Supports),
            utils::recoverArgumentLabelling(ArgsIn, ArgsOut, ArgsUnd),
            utils::recoverStatementLabelling(StatIn, StatOut, StatUnd).

        buildGraph :-
            setting(graphBuildMode, X),
            X:::buildArgumentationGraph.

        modifyGraph :-
            findall(X, setting(graphExtension, X), Ext),
            modifyGraph(Ext).

        modifyGraph([]).
        modifyGraph([X|Ext]) :-
            modifyGraph(Ext),
            X:::modifyArgumentationGraph.

        buildArgumentLabelling :-
            setting(argumentLabellingMode, X),
            X:::argumentLabelling.

        buildStatementLabelling :-
            setting(statementLabellingMode, X),
            X:::statementLabelling.

        solve(Arguments, Attacks, I, O, U) :-
            solve(Arguments, Attacks),
            utils::recoverArgumentLabellingAbstract(I, O, U).

        % The lists are visited natively (for_each): as the goal of a findall, every step of the saving would visit them
        solve(Arguments, Attacks) :-
            context_reset,
            utils::for_each(A, Arguments,
                standard_af::saveArgument(graph, [A], [[A], A, [A], [], [[], [], []]])
            ),
            utils::for_each((X, Y), Attacks, (
                XX = [[X], X, [X], [], [[], [], []]],
                YY = [[Y], Y, [Y], [], [[], [], []]],
                standard_af::saveAttack(abstract, XX, YY, YY)
            )),
            buildArgumentLabelling.
        """.trimIndent()
}

object GraphExtension : ArgsFlag<Iterable<String>, Iterable<String>> {
    override fun predicate(): String = "graphExtension"

    override fun default(): Iterable<String> = listOf("standardPref")

    override fun values(): Iterable<String> = listOf("rebutRestriction", "bp", "standardPref", "defeasiblePref", "defeasibleAllPref")
}

object ArgumentLabellingMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "argumentLabellingMode"

    override fun default(): String = "grounded"

    override fun values(): Iterable<String> =
        listOf(
            "conflictfree",
            "admissible",
            "stronglyadmissible",
            "grounded",
            "complete",
            "preferred",
            "semistable",
            "stable",
            "ideal",
            "eager",
            "naive",
            "stage",
            "cf2",
            "stage2",
            "bp_grounded",
            "bp_grounded_partial",
            "bp_grounded_complete",
            "grounded_old",
        )
}

object StatementLabellingMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "statementLabellingMode"

    override fun default(): String = "statement"

    override fun values(): Iterable<String> = listOf("statement", "statement_pass_through", "statement_binary")
}

object GraphBuildMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "graphBuildMode"

    override fun default(): String = "standard_af"

    override fun values(): Iterable<String> = listOf("standard_af")
}
