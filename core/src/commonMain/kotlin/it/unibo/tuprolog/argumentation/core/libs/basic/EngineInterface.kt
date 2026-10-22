package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.core.operators.OperatorSet

object EngineInterface : PrologModule("prolog.argumentation.interface", "arg2p") {
    // The operators of the rules of a theory, for the theory of the interface and the solvers using it
    override val libraryOperators: OperatorSet
        get() = RuleParserBase.operators()

    override val theoryOperators: OperatorSet
        get() = super.theoryOperators.plus(RuleParserBase.operators())

    override val prologRawTheory: String =
        """
        argTuProlog.

        solve :- buildLabelSetsSilent.
        solve(Query) :- answerQuery(Query).
        solve(Query, Res) :- answerQuery(Query, Res).

        buildLabelSets([StatIn, StatOut, StatUnd], [ArgsIn, ArgsOut, ArgsUnd]) :-
            context_reset,
            parser:::convertAllRules(_),
            abstract::computeGlobalAcceptance(_, [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]).

        buildLabelSets(StatIn, StatOut, StatUnd) :-
            buildLabelSets([StatIn, StatOut, StatUnd], _).

        buildLabelSets :-
            context_reset,
            parser:::convertAllRules(ArgRules),
            abstract::computeGlobalAcceptance([Arguments, Attacks, Supports], [ArgsIn, ArgsOut, ArgsUnd], [StatIn, StatOut, StatUnd]),
            debug::printArgumentationGraph(Arguments, Attacks, Supports),
            debug::printArgumentLabelling([ArgsIn, ArgsOut, ArgsUnd]),
            debug::printStatementLabelling([StatIn, StatOut, StatUnd]).

        answerQuery(Goal, Yes, No, Und) :-
            context_reset,
            parser:::convertAllRules(_),
            structured:::computeStatementAcceptance(Goal, Yes, No, Und).

        answerQuery(Goal, Res) :-
            context_reset,
            parser:::convertAllRules(_),
            structured:::computeStatementAcceptance(Goal, Res).

        answerQuery(Goal) :-
            context_reset,
            parser:::convertAllRules(_),
            structured:::computeStatementAcceptance(Goal),
            context_check(statIn([Goal])).

        buildLabelSetsSilent :-
            context_reset,
            parser:::convertAllRules(ArgRules),
            abstract::computeGlobalAcceptance.
        """.trimIndent()
}
