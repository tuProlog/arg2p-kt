package it.unibo.tuprolog.argumentation.core.modules.entry

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.modules.language.ParserModule
import it.unibo.tuprolog.core.operators.OperatorSet

/**
 * The entry points of arg2p (module `arg2p`, loaded by every solver): the labels of the statements of the theory,
 * of the whole graph or of a query.
 */
object Arg2pModule : PrologModule("prolog.argumentation.interface", "arg2p") {
    // The operators of the rules of a theory, for the theory of the interface and the solvers using it
    override val libraryOperators: OperatorSet
        get() = ParserModule.operators()

    override val theoryOperators: OperatorSet
        get() = super.theoryOperators.plus(ParserModule.operators())

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! argTuProlog
        %  The solver is an arg2p one.
        argTuProlog.

        %! solve
        %  The labelling of the theory, in the context (buildLabelSets/0, without printing it).
        solve :- buildLabelSetsSilent.
        %! solve(+Query)
        %  answerQuery/1.
        solve(Query) :- answerQuery(Query).
        %! solve(+Query, -Result)
        %  answerQuery/2.
        solve(Query, Res) :- answerQuery(Query, Res).

        %! buildLabelSets(-StatementLabels, -ArgumentLabels)
        %  The statements and the arguments of the theory labelled IN, OUT and UND, as [In, Out, Und] lists.
        buildLabelSets([StatIn, StatOut, StatUnd], [ArgsIn, ArgsOut, ArgsUnd]) :-
            context_reset,
            parser:::convertAllRules(_),
            abstract::computeGlobalAcceptance(_, [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]).

        %! buildLabelSets(-StatIn, -StatOut, -StatUnd)
        %  The statements of the theory labelled IN, OUT and UND.
        buildLabelSets(StatIn, StatOut, StatUnd) :-
            buildLabelSets([StatIn, StatOut, StatUnd], _).

        %! buildLabelSets
        %  The labelling of the theory, printed: the graph, the labels of the arguments and those of the statements.
        buildLabelSets :-
            context_reset,
            parser:::convertAllRules(ArgRules),
            abstract::computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]),
            debug::printArgumentationGraph(Arguments, Attacks, Supports),
            debug::printArgumentLabelling([ArgsIn, ArgsOut, ArgsUnd]),
            debug::printStatementLabelling([StatIn, StatOut, StatUnd]).

        %! answerQuery(+Goal, -Yes, -No, -Und)
        %  The statements unifying with Goal labelled IN, OUT and UND: with queryMode only the part of the graph they depend on is built.
        answerQuery(Goal, Yes, No, Und) :-
            context_reset,
            parser:::convertAllRules(_),
            structured:::computeStatementAcceptance(Goal, Yes, No, Und).

        %! answerQuery(+Goal, -Result)
        %  The labels of the statements unifying with Goal, as in(S), out(S) and und(S) terms.
        answerQuery(Goal, Res) :-
            context_reset,
            parser:::convertAllRules(_),
            structured:::computeStatementAcceptance(Goal, Res).

        %! answerQuery(+Goal)
        %  Goal is a statement labelled IN.
        answerQuery(Goal) :-
            context_reset,
            parser:::convertAllRules(_),
            structured:::computeStatementAcceptance(Goal),
            graphStore::statementLabel(in, [Goal]).

        %! buildLabelSetsSilent
        %  The labelling of the theory, in the context.
        buildLabelSetsSilent :-
            context_reset,
            parser:::convertAllRules(ArgRules),
            abstract::computeGlobalAcceptance.
        """.trimIndent()
}
